import Control.Applicative (pure)
import Data.Function (($))
import Data.String (String)
import System.IO (IO)
import Test.Tasty
import Test.Tasty.MGolden

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "mgolden tests"
  [ goldenTest path path $ pure "foo\nbar\n"
  ]
  where
    path :: String
    path = "test/expected/foo.txt"
