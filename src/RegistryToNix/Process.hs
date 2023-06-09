{-# LANGUAGE DataKinds #-}

module RegistryToNix.Process where

import Control.Monad
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map as M
import Data.String.Interpolate
import Data.Text as T
import RegistryToNix.Types
import RegistryToNix.VersionCache
import System.Exit
import System.FilePath
import System.Process
import Test.Sandwich
import qualified Toml
import UnliftIO.Exception


processPackage :: (
  MonadUnliftIO m, MonadLogger m, MonadThrow m, MonadReader ctx m, HasLabel ctx "failureFn" (Package -> IO ()), HasLabel ctx "versionCache" VersionCache
  ) => Package -> m ()
processPackage package@(Package {packageVersions=(Versions versions), ..}) = do
  onFailure <- getContext failureFn
  VersionCache vc <- getContext versionCache

  flip onException (liftIO $ onFailure package) $ do
    PackageInfo {..} <- Toml.decodeFileEither (Toml.genericCodec @PackageInfo) (T.unpack packageFullPath </> "Package.toml") >>= \case
      Left err -> expectationFailure [i|Failed to parse #{T.unpack packageFullPath </> "Package.toml"}: #{err}|]
      Right x -> return x

    debug [i|Processing #{packageName} (#{repo})|]

    versions' <- (M.fromList <$>) $ forM (M.toList versions) $ \(k, version@(Version {..})) -> do
      case nixSha256 of
        Just _ -> return (k, version) -- No need to re-fetch
        Nothing
          | Just sha256 <- M.lookup (packageUuid, k, gitTreeSha1) vc -> return (k, version { nixSha256 = Just sha256 })
          | otherwise -> do
              debug [i|Fetching version #{k}|]
              (exitCode, sout, serr) <- liftIO $ readCreateProcessWithExitCode (
                (proc "nix-prefetch-git" [T.unpack repo, "--rev", T.unpack gitTreeSha1]) {
                    env = Just [("GIT_TERMINAL_PROMPT", "0")]
                    }) ""

              case exitCode of
                ExitSuccess -> case A.eitherDecode $ BL.pack sout of
                  Left err -> expectationFailure [i|Failed to decode nix-prefetch-git output: #{err}. Stdout: #{sout}. Stderr: #{serr}|]
                  Right (NixPrefetchGit {..}) -> return (k, version { nixSha256 = Just sha256 })
                ExitFailure n -> expectationFailure [i|Failed to nix-prefetch-git #{repo} --rev #{gitTreeSha1} (exited with #{n}). Stdout: #{sout}. Stderr: #{serr}|]

    liftIO $ writeVersionsToml (T.unpack packageFullPath </> "Versions.toml") (Versions versions')
