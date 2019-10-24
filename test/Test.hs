import MPrelude
import System.IO (IO)
import Test.Tasty
import Test.Tasty.MGolden

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "mgolden tests"
  [ goldenTest "test/expected/foo.txt" $ pure "foo\nbar\n"
  ]
