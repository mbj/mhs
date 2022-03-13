module Devtools
  ( Config(..)
  , Target(..)
  , defaultConfig
  , defaultMain
  , main
  , testTree
  )
where

import Devtools.Config
import Devtools.Prelude

import qualified Devtools.Dependencies as Dependencies
import qualified Devtools.HLint        as HLint
import qualified Test.Tasty            as Tasty

defaultConfig :: Config
defaultConfig = Config
  { hlintArguments = []
  , targets        = []
  , stackFlags     = []
  }

defaultMain :: IO ()
defaultMain = main defaultConfig

main :: Config -> IO ()
main = Tasty.defaultMain <=< testTree

testTree :: Config -> IO Tasty.TestTree
testTree Config{..} = do
  filename <- Dependencies.getFilename

  pure $ Tasty.testGroup
    "devtools"
    [ Dependencies.testTree filename targets stackFlags
    , HLint.testTree hlintArguments
    ]
