module Main
  ( main
  )
where

import           Control.Monad.Reader
import           MRIO.Core
import           Prelude

import qualified Devtools

newtype Env = Env
  { appName :: String
  }

main :: IO ()
main = do
  runRIO env $ do
    name <- asks appName
    liftIO . putStrLn $ "App Name is " <> name

  Devtools.main Devtools.defaultConfig
 where
  env :: Env
  env = Env { appName = "Test App" }
