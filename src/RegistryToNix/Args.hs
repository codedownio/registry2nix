
module RegistryToNix.Args (
  Args(..)
  , argsParser
  ) where

import Options.Applicative


data Args = Args {
  workRepo :: FilePath
  , numWorkers :: Int
  }

argsParser :: Parser Args
argsParser = Args
  <$> strOption (long "repo" <> short 'r' <> help "Julia registry repo to work on" <> metavar "STRING")
  <*> option auto (long "num-workers" <> short 'w' <> showDefault <> help "Num workers" <> value 100 <> metavar "INT")
