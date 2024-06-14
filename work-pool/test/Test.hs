import Control.Arrow (left)
import Control.Monad (when)
import MPrelude
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.List           as List
import qualified Data.Set            as Set
import qualified Data.String         as String
import qualified Devtools
import qualified UnliftIO.Concurrent as UnliftIO
import qualified UnliftIO.Exception  as UnliftIO
import qualified WorkPool

main :: IO ()
main
  = defaultMain
  $ testGroup "work-pool"
  [ Devtools.testTree $$(Devtools.readDependencies [Devtools.Target "work-pool"])
  , mkSuccess 1 1
  , mkSuccess 1 100
  , mkSuccess 100 1
  , mkSuccess 100 100
  , mkSuccess 1000 1000
  , producerFailure
  , workerFailure
  ]
  where
    mkSuccess :: Natural -> Natural -> TestTree
    mkSuccess queueSize workerCount =
      testCase ("queue size: " <> show queueSize <> ", workerCount: " <> show workerCount) $ do
        output <- UnliftIO.newMVar []
        WorkPool.runPool $ config output
        assertEqual "" (Set.fromList values) =<< UnliftIO.readMVar output
      where
        config output = WorkPool.Config{..}
          where
            workerRun :: Natural -> WorkPool.Source Natural -> IO WorkPool.Done
            workerRun _index = WorkPool.runSource $ \value -> do
              void $ UnliftIO.modifyMVar output $ \set -> pure (Set.insert value set, ())

    workerFailure :: TestTree
    workerFailure = testCase "worker failure" $ do
      result <- UnliftIO.try (WorkPool.runPool config)
      assertEqual "" (Left "intentional error\n") (left formatException result)
      where
        config = WorkPool.Config{queueSize = 100, workerCount = 100, ..}

        workerRun :: Natural -> WorkPool.Source Natural -> IO WorkPool.Done
        workerRun _index =
          WorkPool.runSource $ \value ->
            when (value == 100) $ UnliftIO.throwString "intentional error"

    producerFailure :: TestTree
    producerFailure = testCase "producer failure" $ do
      result <- UnliftIO.try (WorkPool.runPool config)
      assertEqual "" (Left "intentional error\n") (left formatException result)
      where
        config :: WorkPool.Config IO Natural
        config
          = WorkPool.Config
          { produceJobs = produceJobsFailing
          , queueSize   = 100
          , workerCount = 100
          , ..
          }

        produceJobsFailing :: MonadIO m => WorkPool.Pool Natural -> m ()
        produceJobsFailing pool = do
          WorkPool.pushJob pool 1
          UnliftIO.throwString "intentional error"

        workerRun :: MonadIO m => Natural -> WorkPool.Source Natural -> m WorkPool.Done
        workerRun _index = WorkPool.runSource $ const (pure ())

    formatException :: UnliftIO.SomeException -> String
    formatException
      = String.unlines
      . List.drop 2
      . List.take 3
      . String.lines
      . UnliftIO.displayException

    produceJobs :: MonadIO m => WorkPool.Pool Natural -> m ()
    produceJobs pool = traverse_ (WorkPool.pushJob pool) values

    values :: [Natural]
    values = [0..1000]
