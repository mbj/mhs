import MPrelude
import Test.Tasty

import qualified Devtools
import qualified Test.TraceHeader as TraceHeader

main :: IO ()
main = do
  devtools <- Devtools.testTree Devtools.defaultConfig
  defaultMain $ testGroup "xray" [devtools, TraceHeader.testTree]
