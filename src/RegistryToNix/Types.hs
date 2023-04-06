{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module RegistryToNix.Types where

import Control.Monad
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty
import Data.Map as M
import Data.String.Interpolate
import Data.Text
import GHC.Generics
import Toml
import Toml.Codec

import Data.Text.IO as T
import Control.Exception
import Validation

-- * Registry

data Registry = Registry {
  registryName :: Text
  , registryPackages :: Packages
  }
  deriving (Show, Generic)
registryCodec :: TomlCodec Registry
registryCodec = stripTypeNameCodec

data Packages = Packages {
  packagesItems :: Map Text NameAndPath
  } deriving (Show, Generic)
packagesCodec :: TomlCodec Packages
packagesCodec = Packages
  <$> Toml.tableMap Toml._KeyText (hasCodec @NameAndPath) "packages" .= packagesItems
instance HasCodec Packages where
  hasCodec _ = packagesCodec

data NameAndPath = NameAndPath {
  name :: Text
  , path :: Text
  } deriving (Show, Generic)
instance HasCodec NameAndPath where
  hasCodec = Toml.table genericCodec

-- * Versions

data Versions = Versions {
  versions :: Map Text Version
  } deriving (Show, Generic)
-- instance HasCodec Versions where
--   hasCodec = match _Versions

-- _Versions :: TomlBiMap Versions AnyValue
-- _Versions = BiMap forward backward where
--   forward :: Versions -> Either TomlBiMapError AnyValue
--   forward (Versions versions) = undefined

--   backward :: AnyValue -> Either TomlBiMapError Versions
--   backward = undefined

data Version = Version {
  gitTreeSha1 :: Text
  , nixSha256 :: Maybe Text
  } deriving (Show, Generic)
instance HasCodec Version where
  hasCodec _ = genericCodecWithOptions $ dashifyOptions
    where
      dashifyOptions :: TomlOptions Version
      dashifyOptions = TomlOptions {
        tomlOptionsFieldModifier = \_ -> \case
            "gitTreeSha1" -> "git-tree-sha1"
            "nixSha256" -> "nix-sha-256"
            x -> error [i|Couldn't dashify: #{x}|]
        }

-- | Having trouble parsing Versions.toml normally due to
-- https://github.com/kowainik/tomland/issues/404,
-- so made this hacky version
parseVersionsToml :: FilePath -> IO (Map Text Version)
parseVersionsToml path = (Toml.parse <$> T.readFile path) >>= \case
  Left err -> throwIO $ userError [i|Failed to parse version file|]
  Right (Toml.TOML {tomlTables}) -> do
    (fmap M.fromList) <$> forM (HM.toList tomlTables) $ \pair@(piece@(Piece t), Leaf k toml) -> do
      case runTomlCodec (hasCodec @Version (Key (piece :| []))) toml of
        Success (version :: Version) -> return (t, version)
        Failure e -> throwIO $ userError [i|Failed to parse version file piece: #{pair}|]

-- * Package

data Package = Package {
  packageName :: Text
  , packagePath :: Text
  , packageVersions :: Versions
  } deriving (Show)
