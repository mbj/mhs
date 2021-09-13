module Main
  ( main
  )
where

import qualified Devtools

main :: IO ()
main
  = Devtools.main Devtools.defaultConfig
  { Devtools.targets = [Devtools.Target "lambda-alb"] }
