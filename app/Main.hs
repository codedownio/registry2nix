
module Main (main) where

import Control.Concurrent.QSem
import Control.Monad
import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.List as L
import Data.Map as M
import qualified Data.Set as Set
import Data.String.Interpolate
import Data.Text as T
import Data.Time
import RegistryToNix.Args
import RegistryToNix.Types
import System.Exit
import System.FilePath
import System.Process
import Test.Sandwich
import qualified Toml
import UnliftIO.Exception


main :: IO ()
main = do
  CommandLineOptions {optUserOptions=(Args {..})} <- parseCommandLineArgs argsParser (return ())

  Registry {..} <- Toml.decodeFileEither registryCodec (workRepo </> "Registry.toml") >>= \case
    Left err -> throwIO $ userError [i|Failed to parse #{workRepo </> "Registry.toml"}: #{err}|]
    Right x -> return x

  allPackages <- forM (M.toList (packagesItems registryPackages)) $ \(_uuid, NameAndPath n p) ->
    case splitPath $ T.unpack p of
      (x:_:_) -> do
        versions <- parseVersionsToml (workRepo </> T.unpack p </> "Versions.toml")
        return $ Package {
          packageName = n
          , packagePath = p
          , packageFullPath = T.pack (workRepo </> T.unpack p)
          , packageVersions = Versions versions
          }

  let incompletePackages = L.filter (not . isCompletePackage) allPackages

  putStrLn [i|Found #{L.length incompletePackages} packages to process out of #{L.length allPackages} total|]

  runSandwichWithCommandLineArgs' testOptions argsParser $ do
    introduce' (defaultNodeOptions { nodeOptionsCreateFolder = False }) "Introduce parallel semaphore" parallelSemaphore (liftIO $ newQSem numWorkers) (const $ return ()) $
      treeToSpec (treeifyPackages incompletePackages)

data Tree a =
  DescribeNode Text [Tree a]
  | LeafNode a

treeifyPackages :: [Package] -> Tree Package
treeifyPackages packages = DescribeNode "Root" folderLevelNodes
  where
    folderLevelNodes = [getFolderLevelNode (T.pack folder) | folder <- Set.toList topLevelFolders]
    topLevelFolders = Set.fromList [x | Package {packagePath=(splitPath . T.unpack -> (x:_:_))} <- packages]

    getFolderLevelNode :: Text -> Tree Package
    getFolderLevelNode folder = DescribeNode folder (fmap LeafNode packagesInFolder)
      where
        packagesInFolder = [package | package@(Package {packagePath}) <- packages, folder `T.isPrefixOf` packagePath]

treeToSpec :: (MonadThrow m, MonadIO m, MonadUnliftIO m, MonadMask m, HasParallelSemaphore ctx) => Tree Package -> SpecFree ctx m ()
treeToSpec (DescribeNode label subtree) = describe (T.unpack label) (parallel (L.foldl' (>>) (return ()) (fmap treeToSpec subtree)))
treeToSpec (LeafNode package@(Package {packageName})) = withParallelSemaphore $ it [i|#{packageName}|] $ processPackage package

processPackage (Package {packageVersions=(Versions versions), ..}) = do
  PackageInfo {..} <- Toml.decodeFileEither (Toml.genericCodec @PackageInfo) (T.unpack packageFullPath </> "Package.toml") >>= \case
    Left err -> expectationFailure [i|Failed to parse #{T.unpack packageFullPath </> "Package.toml"}: #{err}|]
    Right x -> return x

  debug [i|Processing #{packageName} (#{repo})|]

  versions' <- (M.fromList <$>) $ forM (M.toList versions) $ \(k, version@(Version {..})) -> do
    debug [i|Fetching version #{k}|]
    (exitCode, sout, serr) <- liftIO $ readCreateProcessWithExitCode (proc "nix-prefetch-git" [T.unpack repo, "--rev", T.unpack gitTreeSha1]) ""
    case exitCode of
      ExitSuccess -> case A.eitherDecode $ BL.pack sout of
        Left err -> expectationFailure [i|Failed to decode nix-prefetch-git output. Stdout: #{sout}. Stderr: #{serr}|]
        Right (NixPrefetchGit {..}) -> return (k, version { nixSha256 = Just sha256 })
      ExitFailure n -> expectationFailure [i|Failed to nix-prefetch-git #{repo} --rev #{gitTreeSha1} (exited with #{n}). Stdout: #{sout}. Stderr: #{serr}|]

  liftIO $ writeVersionsToml (T.unpack packageFullPath </> "Versions.toml") (Versions versions')

isCompletePackage :: Package -> Bool
isCompletePackage (Package {packageVersions=(Versions versions)}) = Prelude.all (hasNixSha256 . snd) (M.toList versions)
  where
    hasNixSha256 (Version {nixSha256=(Just _)}) = True
    hasNixSha256 _ = False

testOptions :: Options
testOptions = defaultOptions {
  optionsTestArtifactsDirectory = TestArtifactsGeneratedDirectory "test_runs" (show <$> getCurrentTime)
  }

withParallelSemaphore :: forall context m. (
  MonadUnliftIO m, MonadIO m, MonadMask m, HasParallelSemaphore context
  ) => SpecFree context m () -> SpecFree context m ()
withParallelSemaphore = around' (defaultNodeOptions { nodeOptionsRecordTime = False
                                                    , nodeOptionsVisibilityThreshold = 125
                                                    , nodeOptionsCreateFolder = False }) "claim semaphore" $ \action -> do
  s <- getContext parallelSemaphore
  bracket_ (liftIO $ waitQSem s) (liftIO $ signalQSem s) (void action)

-- test :: IO ()
-- test = runStderrLoggingT $ do
--   let package = Package {packageName = "REPLTreeViews", packagePath = "/home/tom/tools/General/R/REPLTreeViews", packageVersions = Versions {versions = fromList [("\"0.1.0\"",Version {gitTreeSha1 = "4b4995d67c3bac2c790a56928dcb0b8c014d8c66", nixSha256 = Nothing})]}}
--   processPackage package
