
module Main (main) where

import Control.Concurrent.QSem
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import Data.Map as M
import qualified Data.Set as Set
import Data.String.Interpolate
import Data.Text as T
import Data.Time
import qualified Data.Yaml as Yaml
import qualified Data.Yaml.Pretty as Yaml
import RegistryToNix.Args
import RegistryToNix.Tree
import RegistryToNix.Types
import RegistryToNix.Util
import RegistryToNix.VersionCache
import System.FilePath
import Test.Sandwich
import Test.Sandwich.Formatters.TerminalUI
import qualified Toml
import UnliftIO.Concurrent
import UnliftIO.Exception


main :: IO ()
main = do
  CommandLineOptions {optUserOptions=(Args {..})} <- parseCommandLineArgs argsParser (return ())

  Registry {..} <- Toml.decodeFileEither registryCodec (workRepo </> "Registry.toml") >>= \case
    Left err -> throwIO $ userError [i|Failed to parse #{workRepo </> "Registry.toml"}: #{err}|]
    Right x -> return x

  previousFailuresSet :: Set.Set PreviousFailure <- case ignoreFailures of
    Nothing -> return mempty
    Just path -> Yaml.decodeFileEither path >>= \case
      Left err -> throwIO $ userError [i|Failed decode previous failures in #{path}: #{err}|]
      Right xs -> pure xs

  let previousFailures = M.fromListWith Set.union [(uuid, Set.singleton failureInfo)
                                                  | (PreviousFailure uuid failureInfo) <- Set.toList previousFailuresSet]

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

  vc <- case initialRepo of
    Nothing -> return $ VersionCache mempty
    Just p -> buildVersionCache p

  let incompletePackages = L.filter (\x -> not (isIgnoredPackage previousFailures x || isCompletePackage x)) allPackages

  putStrLn [i|Found #{L.length incompletePackages} packages to process out of #{L.length allPackages} total|]

  failuresVar :: MVar (Set.Set PreviousFailure) <- newMVar mempty

  let onFailure (Package {..}) failureInfo = modifyMVar_ failuresVar (pure . Set.insert (PreviousFailure packageUuid failureInfo))

  let writeNewFailures = case writeFailures of
        Nothing -> pure ()
        Just path -> do
          allNewFailures <- readMVar failuresVar
          B.writeFile path (Yaml.encodePretty (Yaml.setConfCompare putUuidFirst Yaml.defConfig) (previousFailuresSet <> allNewFailures))

  flip finally writeNewFailures $
    runSandwichWithCommandLineArgs' testOptions argsParser $ do
      introduce "Failure function" failureFn (pure onFailure) (const $ return ()) $
        introduce "VersionCache" versionCache (pure vc) (const $ return ()) $
          introduce' (defaultNodeOptions { nodeOptionsCreateFolder = False }) "Introduce parallel semaphore" parallelSemaphore (liftIO $ newQSem numWorkers) (const $ return ()) $
            treeToSpec (treeifyPackages incompletePackages)


putUuidFirst :: Text -> Text -> Ordering
putUuidFirst "uuid" _ = GT
putUuidFirst _ "uuid" = LT
putUuidFirst x y = x `compare` y

testOptions :: Options
testOptions = defaultOptions {
  optionsTestArtifactsDirectory = TestArtifactsGeneratedDirectory "test_runs" (show <$> getCurrentTime)
  , optionsFormatters = [SomeFormatter $ defaultTerminalUIFormatter {
      terminalUIInitialFolding = InitialFoldingAllClosed
      }]
  }
