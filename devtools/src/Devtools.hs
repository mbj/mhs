module Devtools
  ( Devtools.Target(..)
  , main
  , readDependencies
  , testTree
  )
where

import Devtools.Prelude
import System.IO (putStrLn)

import qualified Devtools.Dependencies      as Devtools
import qualified Devtools.HLint             as HLint
import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Lift   as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified Test.Tasty                 as Tasty

main :: Devtools.Dependencies -> IO ()
main dependencies = do
  putStrLn empty
  Tasty.defaultMain $ testTree dependencies

testTree :: Devtools.Dependencies -> Tasty.TestTree
testTree dependencies =
  Tasty.testGroup
    "devtools"
    [ Devtools.dependencyTestTree dependencies
    , HLint.testTree
    ]

readDependencies :: [Devtools.Target] -> TH.Code TH.Q Devtools.Dependencies
readDependencies
  = TH.liftCode
  . fmap TH.TExp
  . (TH.lift <=< TH.runIO . Devtools.getDependencies)
