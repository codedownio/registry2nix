
module RegistryToNix.Args (
  Args(..)
  , argsParser
  ) where

import Options.Applicative


data Args = Args {
  workRepo :: FilePath
  }

argsParser :: Parser Args
argsParser = Args
  <$> strOption (long "work-repo" <> help "Julia registry repo to work on" <> metavar "STRING")
