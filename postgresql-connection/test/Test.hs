import MPrelude

import qualified Devtools

main :: IO ()
main = Devtools.main devtoolsConfig

devtoolsConfig :: Devtools.Config
devtoolsConfig = Devtools.defaultConfig
  { Devtools.hlintArguments = ["-XTemplateHaskell", "-XTypeApplications"]
  , Devtools.targets        = [Devtools.Target "dbt-postgresql-connection"]
  }
