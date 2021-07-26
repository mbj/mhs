import Data.IORef
import MPrelude
import MRIO.Core
import Test.Tasty
import Test.Tasty.HUnit

import qualified Devtools
import qualified MRIO.Log as Log

data Env = Env
  { logAction :: Log.Action (RIO Env)
  , logBuffer :: IORef [Log.Message]
  , appName   :: Text
  }

main :: IO ()
main = do
  devtools <- Devtools.testTree
    Devtools.defaultConfig
    { Devtools.targets = [Devtools.Target "mrio-log"] }

  defaultMain $ testGroup "mrio-log" [devtools, testEnv]

testEnv :: TestTree
testEnv = testCase "test app" $ do
  env@Env{..} <- mkEnv

  runRIO env runApp

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
       { logAction = \message -> liftIO $ modifyIORef' logBuffer (message:)
       , appName   = "Test App"
       , ..
       }

runApp :: Log.Env env => RIO env ()
runApp = Log.info "Test Log"