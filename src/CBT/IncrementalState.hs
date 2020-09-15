module CBT.IncrementalState
  ( IncrementalState
  , Result(..)
  , new
  , runBuild
  )
where

import CBT.Prelude
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import UnliftIO.MVar (MVar)

import qualified Colog
import qualified Data.HashMap.Strict as HashMap
import qualified UnliftIO.MVar       as MVar

type StateMap a = HashMap a (MVar Result)

newtype IncrementalState a = IncrementalState (MVar (StateMap a))

data Result = Success | Failure

type Key a = (Eq a, Hashable a, Show a)

new :: (Key a , MonadUnliftIO m) => m (IncrementalState a)
new = IncrementalState <$> MVar.newMVar []

data Action = Run (MVar Result) | Wait (MVar Result)

runBuild
  :: forall m env a . (MonadUnliftIO m, Colog.WithLog env Colog.Message m, Key a)
  => IncrementalState a
  -> a
  -> m Result
  -> m ()
runBuild (IncrementalState state) key buildAction = do
  MVar.modifyMVar state process >>= \case
    Run build  -> MVar.putMVar build =<< buildWithLog
    Wait build -> waitResult         =<< (logWait >> MVar.readMVar build)
  where
    waitResult :: Result -> m ()
    waitResult = \case
      Failure -> error                    $ "Build of " <> show key <> " failed in other thread"
      Success -> Colog.logDebug . convert $ "Build of " <> show key <> " succeeded in other thread"

    logWait :: m ()
    logWait = Colog.logDebug . convert $ "Waiting for build: " <> show key

    buildWithLog = do
      Colog.logDebug . convert $ "Starting to build: " <> show key
      output <- buildAction
      Colog.logDebug . convert $ "Finished building: " <> show key
      pure output

    process :: StateMap a -> m (StateMap a, Action)
    process map = maybe (newBuild map) (waitBuild map) (HashMap.lookup key map)

    error :: String -> m ()
    error message = Colog.logError (convert message) >> liftIO (fail message)

    newBuild :: StateMap a -> m (StateMap a, Action)
    newBuild map = do
      build <- MVar.newEmptyMVar
      pure (HashMap.insert key build map, Run build)

    waitBuild :: StateMap a -> MVar Result -> m (StateMap a, Action)
    waitBuild map build = pure (map, Wait build)
