module Main where

import PGT.Prelude
import System.Environment (getArgs)
import System.IO (IO)

import qualified PGT.CLI

-- | Main entry point
main :: IO ()
main = PGT.CLI.run =<< getArgs
