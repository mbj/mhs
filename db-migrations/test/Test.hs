import MPrelude

import qualified Devtools

main :: IO ()
main
  = Devtools.main Devtools.defaultConfig
  { Devtools.targets = [Devtools.Target "db-migrations"] }
