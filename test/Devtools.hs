import MPrelude

import qualified Devtools

main :: IO ()
main
  = Devtools.main
  $ Devtools.defaultConfig { Devtools.hlintArguments = ["-XNumericUnderscores"] }
