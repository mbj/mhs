module Main
  ( main
  )
where

import           Prelude

import qualified Devtools

main :: IO ()
main = Devtools.main
  Devtools.defaultConfig
  { Devtools.targets = [Devtools.Target "mrio-amazonka"]
  }
