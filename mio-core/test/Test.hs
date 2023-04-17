{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Control.Monad.Reader
import           MIO.Core
import           Prelude

import qualified Devtools

newtype Env = Env
  { appName :: String
  }

main :: IO ()
main = do
  runMIO env $ do
    name <- asks (.appName)
    liftIO . putStrLn $ "App Name is " <> name

  Devtools.main $$(Devtools.readDependencies [Devtools.Target "mio-core"])
  where
    env :: Env
    env = Env { appName = "Test App" }
