{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module RegistryToNix.VersionCache where

import Control.Monad
import Control.Monad.IO.Class
import Data.Map as M
import Data.String.Interpolate
import Data.Text as T
import GHC.Stack
import RegistryToNix.Types
import System.FilePath
import Test.Sandwich
import qualified Toml
import UnliftIO.Exception


newtype VersionCache = VersionCache { unVersionCache :: Map (UUID, VersionString, TreeSha1) NixSha256 }

versionCache :: Label "versionCache" VersionCache
versionCache = Label :: Label "versionCache" VersionCache

buildVersionCache :: (HasCallStack, MonadIO m) => FilePath -> m VersionCache
buildVersionCache sourceRepo = do
  Registry {..} <- Toml.decodeFileEither registryCodec (sourceRepo </> "Registry.toml") >>= \case
    Left err -> throwIO $ userError [i|Failed to parse #{sourceRepo </> "Registry.toml"}: #{err}|]
    Right x -> return x

  ((VersionCache . M.fromList . mconcat) <$>) $ forM (M.toList (packagesItems registryPackages)) $ \(uuid, NameAndPath _n p) ->
    case splitPath $ T.unpack p of
      -- Expect path to have at least one directory component
      (_:_:_) -> do
        versions <- liftIO $ parseVersionsToml (sourceRepo </> T.unpack p </> "Versions.toml")
        return [((uuid, versionString, gitTreeSha1), hash) | (versionString, Version {gitTreeSha1, nixSha256=(Just hash)}) <- M.toList versions]
      x -> throwIO $ userError [i|Confused by package path: #{x}|]
