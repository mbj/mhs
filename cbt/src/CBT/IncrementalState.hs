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

import qualified Data.HashMap.Strict as HashMap
import qualified MRIO.Log            as Log
import qualified UnliftIO.Exception  as Exception
import qualified UnliftIO.MVar       as MVar

type StateMap key error success = HashMap key (MVar (Either error success))

newtype IncrementalState key error success = IncrementalState (MVar (StateMap key error success))

type Key key = (Eq key, Hashable key, Show key)

new :: (Key key , MonadUnliftIO m) => m (IncrementalState key error success)
new = IncrementalState <$> MVar.newMVar []

data Action error success = Run (MVar (Either error success)) | Wait (MVar (Either error success))

runBuildThrow
  :: forall env key error success . (Log.Env env, Key key, Exception error)
  => IncrementalState key error success
  -> key
  -> RIO env (Either error success)
  -> RIO env success
runBuildThrow state key buildAction = either Exception.throwIO pure =<< runBuild state key buildAction

runBuild
  :: forall env key error success . (Log.Env env, Key key)
  => IncrementalState key error success
  -> key
  -> RIO env (Either error success)
  -> RIO env (Either error success)
runBuild (IncrementalState state) key buildAction = do
  MVar.modifyMVar state process >>= \case
    Run build  -> buildWithLog build
    Wait build -> waitWithLog build
  where
    waitWithLog :: MVar (Either error success) -> RIO env (Either error success)
    waitWithLog build = do
      Log.debug . convert $ "Waiting for build: " <> show key
      result <- MVar.readMVar build

      either
        (const $ Log.error . convert $ "Build of " <> show key <> " failed in other thread")
        (const $ Log.info . convert $ "Build of " <> show key <> " succeeded in other thread")
        result

      pure result

    buildWithLog :: MVar (Either error success) -> RIO env (Either error success)
    buildWithLog build = do
      Log.debug . convert $ "Starting to build: " <> show key
      result <- buildAction
      MVar.putMVar build result
      logBuildResult result
      pure result

    logBuildResult :: Either error success -> RIO env ()
    logBuildResult result =
      either
        (const . Log.error . convert $ "Failed building: " <> show key)
        (const . Log.info . convert $ "Success building: " <> show key)
        result

    process
      :: StateMap key error success
      -> RIO env (StateMap key error success, Action error success)
    process map = maybe (newBuild map) (waitBuild map) (HashMap.lookup key map)

    newBuild
      :: StateMap key error success
      -> RIO env (StateMap key error success, Action error success)
    newBuild map = do
      build <- MVar.newEmptyMVar
      pure (HashMap.insert key build map, Run build)

    waitBuild
      :: StateMap key error success
      -> MVar (Either error success)
      -> RIO env (StateMap key error success, Action error success)
    waitBuild map build = pure (map, Wait build)
