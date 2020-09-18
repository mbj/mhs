module CBT.IncrementalState
  ( IncrementalState
  , new
  , runBuild
  , runBuildThrow
  )
where

import CBT.Prelude
import Control.Exception (Exception)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import UnliftIO.MVar (MVar)

import qualified Colog
import qualified Data.HashMap.Strict as HashMap
import qualified UnliftIO.Exception  as Exception
import qualified UnliftIO.MVar       as MVar

type StateMap key error success = HashMap key (MVar (Either error success))

newtype IncrementalState key error success = IncrementalState (MVar (StateMap key error success))

type Key key = (Eq key, Hashable key, Show key)

new :: (Key key , MonadUnliftIO m) => m (IncrementalState key error success)
new = IncrementalState <$> MVar.newMVar []

data Action error success = Run (MVar (Either error success)) | Wait (MVar (Either error success))

runBuildThrow
  :: forall m env key error success . (MonadUnliftIO m, Colog.WithLog env Colog.Message m, Key key, Exception error)
  => IncrementalState key error success
  -> key
  -> m (Either error success)
  -> m success
runBuildThrow state key buildAction = either Exception.throwIO pure =<< runBuild state key buildAction

runBuild
  :: forall m env key error success . (MonadUnliftIO m, Colog.WithLog env Colog.Message m, Key key, Show error)
  => IncrementalState key error success
  -> key
  -> m (Either error success)
  -> m (Either error success)
runBuild (IncrementalState state) key buildAction = do
  MVar.modifyMVar state process >>= \case
    Run build  -> buildWithLog build
    Wait build -> waitWithLog build
  where
    waitWithLog :: MVar (Either error success) -> m (Either error success)
    waitWithLog build = do
      Colog.logDebug . convert $ "Waiting for build: " <> show key
      result <- MVar.readMVar build

      either
        (Colog.logError . convert . (("Build of " <> show key <> " failed: ") <>) . show)
        (const $ Colog.logDebug . convert $ "Build of " <> show key <> " succeeded in other thread")
        result

      pure result

    buildWithLog :: MVar (Either error success) -> m (Either error success)
    buildWithLog build = do
      Colog.logDebug . convert $ "Starting to build: " <> show key
      result <- buildAction
      MVar.putMVar build result
      Colog.logDebug . convert $ "Finished building: " <> show key
      pure result

    process
      :: StateMap key error success
      -> m (StateMap key error success, Action error success)
    process map = maybe (newBuild map) (waitBuild map) (HashMap.lookup key map)

    newBuild
      :: StateMap key error success
      -> m (StateMap key error success, Action error success)
    newBuild map = do
      build <- MVar.newEmptyMVar
      pure (HashMap.insert key build map, Run build)

    waitBuild
      :: StateMap key error success
      -> MVar (Either error success)
      -> m (StateMap key error success, Action error success)
    waitBuild map build = pure (map, Wait build)
