import MPrelude

import qualified Devtools

main :: IO ()
main
  = Devtools.main
  $ Devtools.defaultConfig
  { Devtools.hlintArguments = ["-XNumericUnderscores"]
  , Devtools.targets        = [Devtools.Target "stack-deploy"]
  }
