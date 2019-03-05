{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Bag (bagToList)
import Control.Applicative (Alternative(empty), Applicative(pure))
import Control.Monad (mapM, unless)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Bool (Bool(True))
import Data.Eq (Eq((==)))
import Data.Foldable (find)
import Data.Function (($), (.))
import Data.Functor (void)
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup((<>)))
import Data.String (String)
import GHC.Paths (libdir)
import HscMain (hscParse)
import HscTypes (mgModSummaries, msHsFilePath)
import Outputable (($+$), defaultUserStyle, panic, renderWithStyle)
import SourceConstraints (warnings)
import System.IO (IO)
import Test.Hspec (hspec, it, shouldBe)
import Text.Heredoc (str)

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
main = hspec .
  it "returns expected warnings" $ do
    expectWarnings
      "test/MissingDerivingStrategy.hs"
      [[str|Missing deriving strategy
           |  |
           |7 | data Foo = Foo deriving Eq
           |  |                ^^^^^^^^^^^|]]

    expectWarnings
      "test/UnorderedIE.hs"
      [[str|Unsorted import/export, expected: (Integer, (+), head, tail)
           |  |
           |3 | import Prelude (tail, head, Integer, (+))
           |  |                ^^^^^^^^^^^^^^^^^^^^^^^^^^|]]
    expectWarnings
      "test/UnorderedIEThingWith.hs"
      [[str|Unsorted import/export item with list, expected: ((+), (-))
           |  |
           |3 | import GHC.Num (Num((-), (+)))
           |  |                 ^^^^^^^^^^^^^|]]

    expectWarnings
      "test/UnorderedMultipleDeriving.hs"
      [[str|Unsorted multiple deriving, expected: deriving newtype Eq, deriving stock Show
           |   |
           |10 |   deriving stock Show
           |   |   ^^^^^^^^^^^^^^^^^^^...|]]
  where
    expectWarnings file messages = do
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

      mapM (render dynFlags) $ bagToList (warnings dynFlags parsedModule)

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
