
module Main (main) where

import Control.Concurrent.QSem
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.List as L
import Data.Map as M
import qualified Data.Set as Set
import Data.String.Interpolate
import Data.Text as T
import qualified Data.Text.IO as T
import Data.Time
import qualified Data.Yaml as Yaml
import RegistryToNix.Args
import RegistryToNix.Tree
import RegistryToNix.Types
import RegistryToNix.Util
import System.FilePath
import System.IO (hFlush)
import Test.Sandwich
import Test.Sandwich.Formatters.TerminalUI
import qualified Toml
import UnliftIO.Exception


main :: IO ()
main = do
  CommandLineOptions {optUserOptions=(Args {..})} <- parseCommandLineArgs argsParser (return ())

  Registry {..} <- Toml.decodeFileEither registryCodec (workRepo </> "Registry.toml") >>= \case
    Left err -> throwIO $ userError [i|Failed to parse #{workRepo </> "Registry.toml"}: #{err}|]
    Right x -> return x

  uuidsToIgnore :: Set.Set Text <- case ignoreFailures of
    Nothing -> return mempty
    Just path -> Yaml.decodeFileEither path >>= \case
      Left err -> throwIO $ userError [i|Failed decode previous failures in #{path}: #{err}|]
      Right xs -> return $ Set.fromList xs

  allPackages <- forM (M.toList (packagesItems registryPackages)) $ \(uuid, NameAndPath n p) ->
    case splitPath $ T.unpack p of
      -- Expect path to have at least one directory component
      (_:_:_) -> do
        versions <- parseVersionsToml (workRepo </> T.unpack p </> "Versions.toml")
        return $ Package {
          packageName = n
          , packagePath = p
          , packageFullPath = T.pack (workRepo </> T.unpack p)
          , packageVersions = Versions versions
          , packageUuid = uuid
          }
      x -> throwIO $ userError [i|Confused by package path: #{x}|]

  let incompletePackages = L.filter (\x -> not (isIgnoredPackage uuidsToIgnore x || isCompletePackage x)) allPackages

  putStrLn [i|Found #{L.length incompletePackages} packages to process out of #{L.length allPackages} total (ignoring #{L.length uuidsToIgnore})|]

  withMaybeFailureFile writeFailures $ \maybeHandle -> do
    let onFailure (Package {..}) = case maybeHandle of
          Nothing -> return ()
          Just h -> do
            T.hPutStrLn h ("- " <> packageUuid)
            hFlush h

    runSandwichWithCommandLineArgs' testOptions argsParser $ do
      introduce "Failure function" failureFn (pure onFailure) (const $ return ()) $
        introduce' (defaultNodeOptions { nodeOptionsCreateFolder = False }) "Introduce parallel semaphore" parallelSemaphore (liftIO $ newQSem numWorkers) (const $ return ()) $
          treeToSpec (treeifyPackages incompletePackages)


testOptions :: Options
testOptions = defaultOptions {
  optionsTestArtifactsDirectory = TestArtifactsGeneratedDirectory "test_runs" (show <$> getCurrentTime)
  , optionsFormatters = [SomeFormatter $ defaultTerminalUIFormatter {
      terminalUIInitialFolding = InitialFoldingAllClosed
      }]
  }
