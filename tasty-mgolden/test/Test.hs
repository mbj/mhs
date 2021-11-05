import Data.IORef
import Data.Text (Text)
import Prelude
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit
import Test.Tasty.MGolden
import Test.Tasty.Providers.ConsoleFormat

import qualified Data.Text.IO         as Text
import qualified System.Directory     as System
import qualified System.Environment   as System
import qualified System.Exit          as Exit
import qualified System.IO.Temp       as System
import qualified System.Process.Typed as Process

main :: IO ()
main = do
  -- Avoid concurrency as workflow test is unsafe
  System.setEnv "TASTY_NUM_THREADS" "1"
  System.withArgs [] $ defaultMain tests

tests :: TestTree
tests =
  testGroup "mgolden tests"
  [ goldenTest path path $ pure "foo\nbar\n"
  , expectFail $ goldenTest path path $ pure "foo\nbaz\n"
  , testFormat
  , testWorkflow
  ]
  where
    path :: String
    path = "test/expected/example.txt"

testFormat :: TestTree
testFormat = testCase "diff format" $ do
  ioRef <- newIORef id
  runPrinter $ printDetails (fakePutStrLn ioRef) "foo\nbar" "foo\nbaz"
  assertEqual "expected diff format" [" foo", "-bar", "+baz"] =<< readPuts ioRef

fakePutStrLn :: IORef ([Text] -> [Text]) -> Text -> IO ()
fakePutStrLn ioRef text =
  modifyIORef ioRef $ \current -> current . (text :)

readPuts :: IORef ([Text] -> [Text]) -> IO [Text]
readPuts ioRef = ($ []) <$> readIORef ioRef

runPrinter :: ResultDetailsPrinter -> IO ()
runPrinter (ResultDetailsPrinter action) = action 0 (const id)

testWorkflow :: TestTree
testWorkflow =
  testCase "with update" $ do
    System.withSystemTempDirectory "tasty-mgolden" $ \directory -> do
      System.withCurrentDirectory directory $ do
        setup
        assertEqual "initialy exits with tatus 1" (Exit.ExitFailure 1) =<< run []
        assertEqual "exits 0 with update flag" Exit.ExitSuccess        =<< run ["--update"]
        assertEqual "has updated contents" "foo\nbaz\n"                =<< Text.readFile "example/example-b.txt"

run :: [String] -> IO Exit.ExitCode
run arguments = do
  (status, _, _) <-
    Process.readProcess
      $ Process.proc "stack"
        (["exec", "--", "tasty-mgolden-example"] <> arguments)
  pure status

setup :: IO ()
setup = do
  System.createDirectory "example"
  Text.writeFile "example/example-a.txt" "foo\nbar"
  Text.writeFile "example/example-b.txt" "foo\nbar"
