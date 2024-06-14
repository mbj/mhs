module WorkPool
  ( Config(..)
  , Done
  , Pool
  , Source
  , pushJob
  , readJobCount
  , runPool
  , runSource
  )
where

import Data.Set (Set)
import MPrelude
import Prelude (succ)

import qualified Data.Set       as Set
import qualified UnliftIO.Async as UnliftIO
import qualified UnliftIO.STM   as UnliftIO

-- | Worker pool configuration
data Config m a = Config
  { produceJobs :: Pool a -> m ()
  -- ^ function called to produce work, use `pushJob` to create workable jobs.
  -- once this function returns and all jobs are worked off the workers exit.
  , queueSize   :: Natural
  -- ^ maximum size of the jobs queued
  , workerCount :: Natural
  -- ^ number of workers to boot
  , workerRun   :: Natural -> Source a -> m Done
  -- ^ function called when a worker is booted, argument is the worker index,
  -- and a source to be drained with `runSource`
  }

-- Internal queue event, supplying job or quitting the worker
data Event a = Quit | Job a

-- | Running pool
newtype Pool a = Pool
  { queue :: UnliftIO.TBQueue (Event a)
  }

-- | Source to be drained
newtype Source a = Source
  { queue :: UnliftIO.TBQueue (Event a)
  }

-- | Type forcing clients to drain the source via `runSource`
data Done = Done

-- | Add (dynamically) created a job to the pool
--
-- This function will block if the max queue size would be overflown.
-- As the workers create space in the queue this function will unblock.
pushJob :: MonadIO m => Pool a -> a -> m ()
pushJob Pool{..} item
  = UnliftIO.atomically
  $ UnliftIO.writeTBQueue queue (Job item)

-- | Read the number of jobs in the pool
--
-- This function does not block. Its not guaranteed count is still correct
-- when the function returns
readJobCount :: MonadIO m => Pool a -> m Natural
readJobCount Pool{..}
  = UnliftIO.atomically
  $ UnliftIO.lengthTBQueue queue

-- | Drain a source
--
-- This function:
-- * is executed 0 or many times per worker.
-- * executes the action as long there are items in the queue.
-- * as long the producer did not exit: blocks if there are no items in the queue
-- * if the producer exits and the queue is empty: returns.
-- * is he only way to produce a `Done` value required by the `Config` api.
runSource :: MonadIO m => (a -> m ()) -> Source a -> m Done
runSource action Source{..} = go $> Done
  where
    go = UnliftIO.atomically (UnliftIO.readTBQueue queue) >>= \case
      (Job item) -> action item >> go
      Quit       -> UnliftIO.atomically $ UnliftIO.writeTBQueue queue Quit

-- | Run worker pool with specified config
--
-- The function will return if either:
-- * the `produceJobs` function returns
-- * a worker or throws an error
-- * the producer throws an error.
--
-- Care is taken to not leak threads via the use if `withAsync` from the `async` package.
runPool :: forall a m . MonadUnliftIO m => Config m a -> m ()
runPool Config{..} = boot =<< UnliftIO.atomically (UnliftIO.newTBQueue queueSize)
  where
    boot queue = go 0 []
      where
        go index workerHandlers =
          if index == workerCount
            then
              UnliftIO.withAsync
                (runProducer queue)
                (\producerHandle -> waitAll (Set.fromList (producerHandle:workerHandlers)))
            else
              UnliftIO.withAsync
                (workerRun index Source{..})
                (\async -> go (succ index) (async:workerHandlers))

    runProducer queue = do
      produceJobs Pool{..}
      UnliftIO.atomically (UnliftIO.writeTBQueue queue Quit)
      pure Done

    waitAll :: Set (UnliftIO.Async Done) -> m ()
    waitAll remaining = case Set.toList remaining of
      []        -> pure ()
      [handler] -> void $ UnliftIO.wait handler
      list      -> do
        (handler, _result) <- UnliftIO.waitAny list
        waitAll $ Set.delete handler remaining
