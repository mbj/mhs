module Devtools (Config(..), defaultConfig, main, testTree) where

import Control.Applicative (empty)
import Data.Function (($))
import Data.String (String)
import System.IO (IO, putStrLn)

import qualified Devtools.Dependencies as Dependencies
import qualified Devtools.HLint        as HLint
import qualified Test.Tasty            as Tasty

newtype Config = Config
  { hlintArguments :: [String]
  }

defaultConfig :: Config
defaultConfig = Config
  { hlintArguments = []
  }

main :: Config -> IO ()
main config = do
  putStrLn empty
  Tasty.defaultMain $ testTree config

testTree :: Config -> Tasty.TestTree
testTree Config{..} = Tasty.testGroup "devtools"
  [ Dependencies.testTree
  , HLint.testTree hlintArguments
  ]
