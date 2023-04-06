
module Main (main) where

import Control.Exception
import Control.Monad
import Data.Map as M
import qualified Data.Set as Set
import Data.String.Interpolate
import Data.Text as T
import Data.Time
import RegistryToNix.Args
import RegistryToNix.Types
import System.FilePath
import Test.Sandwich
import qualified Toml


main :: IO ()
main = do
  CommandLineOptions {optUserOptions=(Args {..})} <- parseCommandLineArgs argsParser (return ())

  Registry {..} <- Toml.decodeFileEither registryCodec (workRepo </> "Registry.toml") >>= \case
    Left err -> throwIO $ userError [i|Failed to parse #{workRepo </> "Registry.toml"}: #{err}|]
    Right x -> return x
  let topLevelFolders = Set.fromList [x | (_, NameAndPath _ (splitPath . T.unpack -> (x:_:_))) <- M.toList $ packagesItems registryPackages]
  putStrLn [i|topLevelFolders: #{topLevelFolders}|]

  runSandwichWithCommandLineArgs' testOptions argsParser $ do
    forM_ topLevelFolders $ \folder -> do
      it [i|#{folder}|] $ do
        2 `shouldBe` (2 :: Int)

testOptions :: Options
testOptions = defaultOptions {
  optionsTestArtifactsDirectory = TestArtifactsGeneratedDirectory "test_runs" (show <$> getCurrentTime)
  }
