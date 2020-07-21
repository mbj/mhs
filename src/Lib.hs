module Lib
  (someFunc)
where

import MPrelude
import System.IO (putStrLn)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
