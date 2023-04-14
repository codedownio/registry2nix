{-# LANGUAGE DataKinds #-}

module RegistryToNix.Tree where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Unlift
import qualified Data.List as L
import qualified Data.Set as Set
import Data.String.Interpolate
import Data.Text as T
import RegistryToNix.Process
import RegistryToNix.Types
import RegistryToNix.Util
import RegistryToNix.VersionCache
import System.FilePath
import Test.Sandwich


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

treeToSpec :: (
  MonadUnliftIO m, MonadMask m, HasParallelSemaphore ctx, HasLabel ctx "failureFn" (Package -> IO ()), HasLabel ctx "versionCache" VersionCache
  ) => Tree Package -> SpecFree ctx m ()
treeToSpec (DescribeNode label subtree) = describe (T.unpack label) (parallel (L.foldl' (>>) (return ()) (fmap treeToSpec subtree)))
treeToSpec (LeafNode package@(Package {packageName})) = withParallelSemaphore $ it [i|#{packageName}|] $ processPackage package
