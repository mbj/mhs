{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Bag
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Bool
import Data.Eq
import Data.Foldable
import Data.Function
import Data.Maybe
import Data.Semigroup
import Data.String
import GHC.Paths
import HscMain
import HscTypes
import Module
import Outputable hiding ((<>), empty)
import SourceConstraints
import SourceConstraints.LocalModule
import System.IO
import Test.Hspec
import Text.Heredoc

import DynFlags
  ( DynFlags(packageFlags)
  , ModRenaming(ModRenaming)
  , PackageArg(PackageArg)
  , PackageFlag(ExposePackage)
  , getDynFlags
  )

import ErrUtils
  ( WarnMsg
  , errMsgDoc
  , errMsgSeverity
  , errMsgSpan
  , formatErrDoc
  , getCaretDiagnostic
  )

import GHC
  ( GhcMonad
  , LoadHowMuch(LoadAllTargets)
  , depanal
  , getSession
  , getSessionDynFlags
  , guessTarget
  , load
  , runGhc
  , setSessionDynFlags
  , setTargets
  , succeeded
  )

main :: IO ()
main = hspec $ do
  expectWarnings
    "test/MissingDerivingStrategy.hs"
    [[str|Missing deriving strategy
         |  |
         |7 | data Foo = Foo deriving Eq
         |  |                ^^^^^^^^^^^|]]

  expectWarnings
    "test/UnsortedIE.hs"
    [[str|Unsorted import/export declaration, expected: Integer
         |  |
         |4 | import Prelude (tail, head, Integer, (+))
         |  |                 ^^^^|]]
  expectWarnings
    "test/UnsortedIEThingWith.hs"
    [[str|Unsorted import/export item with list, expected: (+)
         |  |
         |3 | import GHC.Num (Num((-), (+)))
         |  |                     ^^^|]]

  expectWarnings
    "test/UnsortedMultipleDeriving.hs"
    [[str|Unsorted deriving clauses, expected: deriving newtype Eq
         |   |
         |10 |   deriving stock Show
         |   |   ^^^^^^^^^^^^^^^^^^^|]]

  expectWarnings
    "test/UnsortedImportStatement.hs"
    [[str|Unsorted import statement, expected: import Data.Bool
         |  |
         |3 | import Data.Char
         |  | ^^^^^^^^^^^^^^^^|]]

  expectWarnings
    "test/LocalModuleExplicitImport.hs"
    [[str|Present import list for local module
         |  |
         |3 | import Data.Word (Word32)
         |  |                  ^^^^^^^^|]]
  where
    expectWarnings file messages =
      it ("returns expected warnings from: " <> file) $ do
        actual <- getWarnings file
        actual `shouldBe` messages

getWarnings :: String -> IO [String]
getWarnings file = runGhc (pure libdir) $ do
  setupDynFlags
  setupTargets

  parseWarnings

  where
    parseWarnings :: GhcMonad m => m [String]
    parseWarnings = do
      moduleGraph <- depanal empty True

      let moduleSummary =
            fromMaybe
              (panic $ "Cannot find module summary for " <> file <> " in dependency graph")
              (find ((== file) . msHsFilePath) $ mgModSummaries moduleGraph)

      env          <- getSession
      dynFlags     <- getDynFlags
      parsedModule <- liftIO $ hscParse env moduleSummary

      let localModules = [LocalModule $ mkModuleName "Data.Word"]

      mapM (render dynFlags) . bagToList . warnings Context{..} $ hpm_module parsedModule

    setupDynFlags :: GhcMonad m => m ()
    setupDynFlags = do
      dynFlags <- getSessionDynFlags
      void . setSessionDynFlags $ dynFlags { packageFlags = [packageFlag] }
      where
        packageFlag =
          ExposePackage
            "-package base"
            (PackageArg "base")
            (ModRenaming True empty)

    setupTargets :: GhcMonad m => m ()
    setupTargets = do
      target <- guessTarget file empty
      setTargets [target]
      result <- load LoadAllTargets

      unless (succeeded result) $ panic "Loading of targets failed"

    render :: GhcMonad m => DynFlags -> WarnMsg -> m String
    render dynFlags warning = do
      caretDiagnostic <- liftIO $
        getCaretDiagnostic
          (errMsgSeverity warning)
          (errMsgSpan warning)

      pure $ renderWithStyle
        dynFlags
        (formatErrDoc dynFlags (errMsgDoc warning) $+$ caretDiagnostic)
        (defaultUserStyle dynFlags)
