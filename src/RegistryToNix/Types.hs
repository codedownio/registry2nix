{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module RegistryToNix.Types where

import Data.Map
import Data.Text
import GHC.Generics
import qualified Toml
import Toml.Codec


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
