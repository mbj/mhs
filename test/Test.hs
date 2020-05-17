import Data.IORef
import Data.Text (Text)
import Prelude
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit
import Test.Tasty.MGolden
import Test.Tasty.Providers.ConsoleFormat

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "mgolden tests"
  [ goldenTest path path $ pure "foo\nbar\n"
  , expectFail $ goldenTest path path $ pure "foo\nbaz\n"
  , testFormat
  ]
  where
    path :: String
    path = "test/expected/foo.txt"

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
