import Data.IORef
import Data.Text (Text)
import Prelude
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit
import Test.Tasty.MGolden
import Test.Tasty.Providers.ConsoleFormat

import qualified Data.Text.IO         as Text
import qualified System.Exit          as Exit
import qualified System.Process.Typed as Process

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "mgolden tests"
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
  testCase "usage workflow" $ do
    reset
    assertEqual "initialy exits with tatus 1" (Exit.ExitFailure 1) =<< run []
    assertEqual "exits 0 with update flag" Exit.ExitSuccess        =<< run ["--update"]
    assertEqual "has updated contents" "foo\nbaz\n"                =<< Text.readFile "example/example-b.txt"
    reset

run :: [String] -> IO Exit.ExitCode
run arguments = do
  (status, _, _) <-
    Process.readProcess
      $ Process.proc "stack"
        (["exec", "--", "tasty-mgolden-example"] <> arguments)
  pure status

reset :: IO ()
reset = do
  Text.writeFile "example/example-a.txt" "foo\nbar"
  Text.writeFile "example/example-b.txt" "foo\nbar"
