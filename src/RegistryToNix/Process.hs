{-# LANGUAGE DataKinds #-}

module RegistryToNix.Process where

import Control.Monad
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger (MonadLogger, MonadLoggerIO)
import Control.Monad.Reader
import qualified Data.Aeson as A
import Data.Map as M
import Data.Maybe
import Data.String.Interpolate
import Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding
import RegistryToNix.Types
import RegistryToNix.VersionCache
import System.Exit
import System.FilePath
import System.Process
import Test.Sandwich
import qualified Toml
import UnliftIO.Exception


processPackage :: (
  MonadUnliftIO m, MonadLoggerIO m, MonadThrow m, MonadReader ctx m
  , HasLabel ctx "failureFn" (Package -> PreviousFailureInfo -> IO ()), HasLabel ctx "versionCache" VersionCache
  ) => Package -> m ()
processPackage package@(Package {packageVersions=(Versions versions), ..}) = do
  onFailure <- getContext failureFn
  VersionCache vc <- getContext versionCache

  flip withException (\(e :: SomeException) -> liftIO $ onFailure package (PreviousFailureInfoUnexpectedFailure (T.pack $ show e))) $ do
    PackageInfo {..} <- Toml.decodeFileEither (Toml.genericCodec @PackageInfo) (T.unpack packageFullPath </> "Package.toml") >>= \case
      Left err -> expectationFailure [i|Failed to parse #{T.unpack packageFullPath </> "Package.toml"}: #{err}|]
      Right x -> return x

    debug [i|Processing #{packageName} (#{repo})|]

    assertCanAccessRepo repo

    versions' <- ((M.fromList . catMaybes) <$>) $ forM (M.toList versions) $ \(k, version@(Version {..})) -> do
      case nixSha256 of
        Just _ -> return $ Just (k, version) -- No need to re-fetch
        Nothing
          | Just sha256 <- M.lookup (packageUuid, k, gitTreeSha1) vc -> return $ Just (k, version { nixSha256 = Just sha256 })
          | otherwise -> do
              handle (\(e :: PreviousFailureInfo) -> liftIO (onFailure package e) >> warn [i|Failed to fetch version: #{k}|] >> pure (Just (k, version))) $ do
                debug [i|Fetching version #{k}|]
                (exitCode, T.pack -> sout, T.pack -> serr) <- liftIO $ readCreateProcessWithExitCode (
                  (proc "nix-prefetch-git" [T.unpack repo, "--rev", T.unpack gitTreeSha1]) {
                      env = Just [("GIT_TERMINAL_PROMPT", "0")]
                      }) ""

                case exitCode of
                  ExitFailure n -> throwIO $ PreviousFailureInfoSpecificVersion k [i|Failed to nix-prefetch-git #{repo} --rev #{gitTreeSha1} (exited with #{n}). Stdout: #{truncateText sout}. Stderr: #{truncateText serr}|]
                  ExitSuccess -> case A.eitherDecode $ encodeUtf8 (TL.fromStrict sout) of
                    Left err -> throwIO $ PreviousFailureInfoSpecificVersion k [i|Failed to decode nix-prefetch-git output: #{err}. Stdout: #{sout}. Stderr: #{serr}|]
                    Right (NixPrefetchGit {..}) -> return $ Just (k, version { nixSha256 = Just sha256 })

    liftIO $ writeVersionsToml (T.unpack packageFullPath </> "Versions.toml") (Versions versions')


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
