{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

module RegistryToNix.Types where

import Control.Exception
import Control.Monad
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty
import Data.Map as M
import Data.String.Interpolate
import Data.Text
import Data.Text.IO as T
import GHC.Generics
import Test.Sandwich (Label(..))
import Toml
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
  versions :: Map VersionString Version
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
  , nixSha256 :: Maybe NixSha256
  } deriving (Show, Generic)
instance HasCodec Version where
  hasCodec _ = genericCodecWithOptions $ dashifyOptions
    where
      dashifyOptions :: TomlOptions Version
      dashifyOptions = TomlOptions {
        tomlOptionsFieldModifier = \_ -> \case
            "gitTreeSha1" -> "git-tree-sha1"
            "nixSha256" -> "nix-sha256"
            x -> error [i|Couldn't dashify: #{x}|]
        }

-- | Having trouble parsing Versions.toml normally due to
-- https://github.com/kowainik/tomland/issues/404,
-- so made this hacky version
parseVersionsToml :: FilePath -> IO (Map VersionString Version)
parseVersionsToml path = (Toml.parse <$> T.readFile path) >>= \case
  Left err -> throwIO $ userError [i|Failed to parse version file: #{err}|]
  Right (Toml.TOML {tomlTables}) -> do
    (fmap M.fromList) <$> forM (HM.toList tomlTables) $ \case
      x@(piece@(Piece t), Leaf _ toml) ->
        case runTomlCodec (hasCodec @Version (Key (piece :| []))) toml of
          Success (version :: Version) -> return (t, version)
          Failure e -> throwIO $ userError [i|Failed to parse version file piece: #{x}: #{e}|]
      x -> throwIO $ userError [i|Unexpected value found in version file: #{x}|]

writeVersionsToml :: FilePath -> Versions -> IO ()
writeVersionsToml path (Versions versions) = T.writeFile path (Toml.pretty toml)
  where
    toml = TOML mempty tables mempty
    tables = HM.fromList $ fmap toPiecePair (M.toList versions)

    toPiecePair :: (Text, Version) -> (Piece, (PrefixTree TOML))
    toPiecePair (k, version) = (Piece k, Leaf key (execTomlCodec (hasCodec @Version key) version))
      where key = Key ((Piece k) :| [])

-- * Package

type UUID = Text
type VersionString = Text
type NixSha256 = Text
type TreeSha1 = Text

data Package = Package {
  packageName :: Text
  , packagePath :: Text
  , packageFullPath :: Text
  , packageVersions :: Versions
  , packageUuid :: UUID
  } deriving (Show)

-- * Repo

data PackageInfo = PackageInfo {
  repo :: Text
  } deriving (Show, Generic)
instance HasCodec PackageInfo where
  hasCodec = Toml.table genericCodec

-- * NixPrefetchGit

data NixPrefetchGit = NixPrefetchGit {
  sha256 :: Text
  } deriving (Generic)
instance A.FromJSON NixPrefetchGit

-- * Contexts

failureFn :: Label "failureFn" (Package -> PreviousFailureInfo -> IO ())
failureFn = Label :: Label "failureFn" (Package -> PreviousFailureInfo -> IO ())

-- * Failures

data PreviousFailure = PreviousFailure {
  uuid :: Text
  , info :: PreviousFailureInfo
  } deriving (Show, Eq, Ord, Generic, A.ToJSON, A.FromJSON)

data PreviousFailureInfo =
  PreviousFailureInfoRepoInaccessible
  | PreviousFailureInfoUnexpectedFailure Text
  | PreviousFailureInfoSpecificVersion VersionString Text
  deriving (Show, Eq, Ord, Generic, Exception, A.ToJSON, A.FromJSON)
