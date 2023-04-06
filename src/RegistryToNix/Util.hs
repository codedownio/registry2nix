
module RegistryToNix.Util where

import Control.Concurrent.QSem
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Data.Map as M
import RegistryToNix.Types
import Test.Sandwich
import UnliftIO.Exception


isCompletePackage :: Package -> Bool
isCompletePackage (Package {packageVersions=(Versions versions)}) = Prelude.all (hasNixSha256 . snd) (M.toList versions)
  where
    hasNixSha256 (Version {nixSha256=(Just _)}) = True
    hasNixSha256 _ = False

withParallelSemaphore :: forall context m. (MonadUnliftIO m, HasParallelSemaphore context) => SpecFree context m () -> SpecFree context m ()
withParallelSemaphore = around' (defaultNodeOptions { nodeOptionsRecordTime = False
                                                    , nodeOptionsVisibilityThreshold = 125
                                                    , nodeOptionsCreateFolder = False }) "claim semaphore" $ \action -> do
  s <- getContext parallelSemaphore
  bracket_ (liftIO $ waitQSem s) (liftIO $ signalQSem s) (void action)

-- test :: IO ()
-- test = runStderrLoggingT $ do
--   let package = Package {packageName = "REPLTreeViews", packagePath = "/home/tom/tools/General/R/REPLTreeViews", packageVersions = Versions {versions = fromList [("\"0.1.0\"",Version {gitTreeSha1 = "4b4995d67c3bac2c790a56928dcb0b8c014d8c66", nixSha256 = Nothing})]}}
--   processPackage package
