{-# LANGUAGE DataKinds #-}

module RegistryToNix.Process where

import Control.Monad
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger (MonadLogger, MonadLoggerIO)
import Control.Monad.Reader
import qualified Data.Aeson as A
import qualified Data.List as L
import Data.Map as M
import Data.Maybe
import qualified Data.Set as Set
import Data.String.Interpolate
import Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding
import RegistryToNix.Types hiding (info)
import RegistryToNix.VersionCache
import System.Exit
import System.FilePath
import System.Process
import Test.Sandwich
import qualified Toml
import UnliftIO.Environment
import UnliftIO.Exception


processPackage :: (
  MonadUnliftIO m, MonadLoggerIO m, MonadThrow m, MonadReader ctx m
  , HasLabel ctx "failureFn" (Package -> PreviousFailureInfo -> IO ()), HasLabel ctx "versionCache" VersionCache
  ) => Package -> Set.Set PreviousFailureInfo -> m ()
processPackage package@(Package {packageVersions=(Versions versions), ..}) previousPackageFailures = do
  onFailure <- getContext failureFn
  VersionCache vc <- getContext versionCache

  let handleException (e :: SomeException) = case fromException e of
        Just (x :: PreviousFailureInfo) -> liftIO $ onFailure package x
        Nothing -> liftIO $ onFailure package (PreviousFailureInfoUnexpectedFailure (T.pack $ show e))

  flip withException handleException $ do
    PackageInfo {..} <- Toml.decodeFileEither (Toml.genericCodec @PackageInfo) (T.unpack packageFullPath </> "Package.toml") >>= \case
      Left err -> expectationFailure [i|Failed to parse #{T.unpack packageFullPath </> "Package.toml"}: #{err}|]
      Right x -> return x

    info [i|Processing #{packageName} (#{repo})|]

    debug [i|Had previous package failures: #{previousPackageFailures}|]

    assertCanAccessRepo repo

    versions' <- ((M.fromList . catMaybes) <$>) $ forM (M.toList versions) $ \(k, version@(Version {..})) -> do
      case nixSha256 of
        Just _ -> return $ Just (k, version) -- No need to re-fetch
        Nothing
          | Just _previousFailure <- findPreviousVersionFailure k previousPackageFailures -> do
              warn [i|Skipping version due to previous failure: #{k}|]
              return $ Just (k, version)
          | Just sha256 <- M.lookup (packageUuid, k, gitTreeSha1) vc ->
              return $ Just (k, version { nixSha256 = Just sha256 })
          | otherwise -> do
              handle (\(e :: PreviousFailureInfo) -> liftIO (onFailure package e) >> warn [i|Failed to fetch version: #{k}|] >> pure (Just (k, version))) $ do
                debug [i|Fetching version #{k}|]
                baseEnv <- getEnvironment
                (exitCode, T.pack -> sout, T.pack -> serr) <- liftIO $ readCreateProcessWithExitCode (
                  (proc "nix-prefetch-git" [T.unpack repo, "--rev", T.unpack gitTreeSha1]) {
                      env = Just (("GIT_TERMINAL_PROMPT", "0") : baseEnv)
                      }) ""

                case exitCode of
                  ExitFailure n -> throwIO $ PreviousFailureInfoSpecificVersion k [i|Failed to nix-prefetch-git #{repo} --rev #{gitTreeSha1} (exited with #{n}). Stdout: #{truncateText sout}. Stderr: #{truncateText serr}|]
                  ExitSuccess -> case A.eitherDecode $ encodeUtf8 (TL.fromStrict sout) of
                    Left err -> throwIO $ PreviousFailureInfoSpecificVersion k [i|Failed to decode nix-prefetch-git output: #{err}. Stdout: #{sout}. Stderr: #{serr}|]
                    Right (NixPrefetchGit {..}) -> return $ Just (k, version { nixSha256 = Just sha256 })

    liftIO $ writeVersionsToml (T.unpack packageFullPath </> "Versions.toml") (Versions versions')


findPreviousVersionFailure :: VersionString -> Set.Set PreviousFailureInfo -> Maybe PreviousFailureInfo
findPreviousVersionFailure versionString failures = case L.filter matches (Set.toList failures) of
  [] -> Nothing
  (x:_) -> Just x
  where
    matches (PreviousFailureInfoSpecificVersion v _reason) = v == versionString
    matches _ = False

assertCanAccessRepo :: (MonadIO m, MonadLogger m) => Text -> m ()
assertCanAccessRepo repo = do
  (exitCode, sout, serr) <- liftIO $ readCreateProcessWithExitCode (
    (proc "git" ["ls-remote", T.unpack repo]) { env = Just [("GIT_TERMINAL_PROMPT", "0")] }) ""
  when (exitCode /= ExitSuccess) $ do
    logError [i|Couldn't access repo '#{repo}'. Code: #{exitCode}. Stdout: '#{sout}'. Stderr: '#{serr}'.|]
    throwIO $ PreviousFailureInfoRepoInaccessible

truncateText :: Text -> Text
truncateText t
  | T.length t > 200 = T.take 200 t <> "..."
  | otherwise = t
