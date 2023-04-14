
module RegistryToNix.Args (
  Args(..)
  , argsParser
  ) where

import Options.Applicative


data Args = Args {
  workRepo :: FilePath
  , numWorkers :: Int
  , ignoreFailures :: Maybe FilePath
  , writeFailures :: Maybe FilePath
  , initialRepo :: Maybe FilePath
  }

argsParser :: Parser Args
argsParser = Args
  <$> strOption (long "repo" <> short 'r' <> help "Julia registry repo to work on" <> metavar "STRING")
  <*> option auto (long "num-workers" <> short 'w' <> showDefault <> help "Num workers" <> value 100 <> metavar "INT")
  <*> optional (strOption (long "ignore-previous-failures" <> short 'i' <> help "Ignore failures from previous .yml file" <> metavar "STRING"))
  <*> optional (strOption (long "write-new-failures" <> short 'f' <> help "Append new failures to .yml file" <> metavar "STRING"))
  <*> optional (strOption (long "initial-repo" <> short 'i' <> help "Initial repo to use as a cache" <> metavar "STRING"))
