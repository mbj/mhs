{-# LANGUAGE ApplicativeDo #-}

module Devtools
  ( Devtools.Target(..)
  , cliParser
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
import qualified Options.Applicative        as CLI
import qualified System.Exit                as System
import qualified Test.Tasty                 as Tasty
import qualified UnliftIO.Environment       as System

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

cliParser :: MonadUnliftIO m => m Tasty.TestTree -> CLI.Parser (m System.ExitCode)
cliParser readTestTree = do
  arguments <- CLI.many (CLI.strArgument $ CLI.metavar "ARGUMENT")
  pure $ System.withArgs arguments ((liftIO . Tasty.defaultMain =<< readTestTree) $> System.ExitSuccess)
