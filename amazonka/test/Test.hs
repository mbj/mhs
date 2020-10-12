module Main
  ( main
  )
where

import           Prelude

import qualified Devtools

main :: IO ()
main = Devtools.main Devtools.defaultConfig
