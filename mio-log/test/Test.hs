{-# LANGUAGE TemplateHaskell #-}

import Data.IORef
import MIO.Core
import MPrelude
import Test.Tasty
import Test.Tasty.HUnit

import qualified Devtools
import qualified MIO.Log as Log

data Env = Env
  { logAction :: Log.Action
  , logBuffer :: IORef [Log.Message]
  , appName   :: Text
  }

main :: IO ()
main
  = defaultMain
  $ testGroup "mio-log" [Devtools.testTree $$(Devtools.readDependencies [Devtools.Target "mio-log"]) , testEnv]

testEnv :: TestTree
testEnv = testCase "test app" $ do
  env@Env{..} <- mkEnv

  runMIO env runApp

  messages <- readIORef logBuffer

  assertEqual
    "expected messages"
    [(Log.Info, "Test Log")]
    ((\Log.Message{..} -> (severity, message)) <$> messages)
 where
   mkEnv :: IO Env
   mkEnv = do
     logBuffer <- newIORef []

     pure $ Env
       { logAction = Log.Action $ \message -> liftIO $ modifyIORef' logBuffer (message:)
       , appName   = "Test App"
       , ..
       }

runApp :: Log.Env env => MIO env ()
runApp = Log.info "Test Log"
