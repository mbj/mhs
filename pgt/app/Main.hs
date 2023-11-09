module Main where

import PGT.Prelude
import System.Environment (getArgs)

import qualified PGT.CLI
import qualified System.Exit as System

-- | Main entry point
main :: IO ()
main = System.exitWith =<< PGT.CLI.run =<< getArgs
