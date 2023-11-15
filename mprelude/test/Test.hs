{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import System.IO (IO)

import qualified Devtools

main :: IO ()
main = Devtools.main $$(Devtools.readDependencies [Devtools.Target "mprelude"])
