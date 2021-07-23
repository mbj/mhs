module Main (main) where

import System.IO (IO)

import qualified Devtools

main :: IO ()
main
  = Devtools.main Devtools.defaultConfig
  { Devtools.targets = [Devtools.Target "devtools"] }
