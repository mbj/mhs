import MPrelude
import Test.Tasty

import qualified Devtools
import qualified Test.TraceHeader as TraceHeader

main :: IO ()
main = do
  devtools <- Devtools.testTree Devtools.defaultConfig { Devtools.targets = [Devtools.Target "xray"] }
  defaultMain $ testGroup "xray" [devtools, TraceHeader.testTree]
