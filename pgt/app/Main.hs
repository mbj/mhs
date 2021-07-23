module Main where

import PGT.Prelude
import System.Environment (getArgs)

import qualified PGT.CLI

-- | Main entry point
main :: IO ()
main = PGT.CLI.run =<< getArgs
