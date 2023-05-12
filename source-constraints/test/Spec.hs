{-# LANGUAGE CPP         #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Bool
import Data.Eq
import Data.Foldable
import Data.Function
import Data.List ((++))
import Data.Maybe
import Data.String
import Data.Traversable
import Prelude (error)
import SourceConstraints
import System.Environment as System
import System.IO
import Test.Hspec
import Text.Heredoc

#if MIN_VERSION_GLASGOW_HASKELL(9,4,0,0)
import GHC
import GHC.Driver.Config.Diagnostic
import GHC.Driver.Errors.Types
import GHC.Driver.Main
import GHC.Driver.Session
import GHC.Paths
import GHC.Types.Error
import GHC.Unit.Module.ModSummary
import GHC.Utils.Error
import GHC.Utils.Outputable
#else
import GHC
import GHC.Data.Bag
import GHC.Driver.Main
import GHC.Driver.Session
import GHC.Paths
import GHC.Unit.Module.ModSummary
import GHC.Utils.Error
import GHC.Utils.Outputable
#endif

main :: IO ()
main = System.withArgs [] . hspec $ do
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
  where
    expectWarnings file messages =
      it ("returns expected warnings from: " ++ file) $ do
        actual <- getWarnings file
        actual `shouldBe` messages

#if MIN_VERSION_GLASGOW_HASKELL(9,4,0,0)
getWarnings :: String -> IO [String]
getWarnings file = runGhc (pure libdir) $ do
  setupDynFlags
  setupTargets

  parseWarnings

  where
    parseWarnings :: GhcMonad m => m [String]
    parseWarnings = do
      moduleGraph <- depanal [] True

      let moduleSummary =
            fromMaybe
              (error $ "Cannot find module summary for " ++ file ++ " in dependency graph")
              (find ((== file) . msHsFilePath) $ mgModSummaries moduleGraph)

      env          <- getSession
      dynFlags     <- getDynFlags
      parsedModule <- liftIO $ hscParse env moduleSummary

      let sDocContext = initSDocContext dynFlags defaultUserStyle
          diagOpts    = initDiagOpts dynFlags

      toList <$> traverse (render sDocContext) (getMessages . warnings Context{..} $ hpm_module parsedModule)

    setupDynFlags :: GhcMonad m => m ()
    setupDynFlags = do
      dynFlags <- getSessionDynFlags
      void . setSessionDynFlags $ dynFlags { packageFlags = [packageFlag] }
      where
        packageFlag =
          ExposePackage
            "-package base"
            (PackageArg "base")
            (ModRenaming True [])

    setupTargets :: GhcMonad m => m ()
    setupTargets = do
      target <- guessTarget file Nothing Nothing
      setTargets [target]
      result <- load LoadAllTargets

      unless (succeeded result) $ error "Loading of targets failed"

    render :: GhcMonad m => SDocContext -> WarnMsg -> m String
    render sDocContext warning = do
      caretDiagnostic <- liftIO $
        getCaretDiagnostic
          (MCDiagnostic (errMsgSeverity warning) WarningWithoutFlag)
          (errMsgSpan warning)

      pure $ renderWithContext
        sDocContext
        (formatBulleted sDocContext (diagnosticMessage $ errMsgDiagnostic warning) $+$ caretDiagnostic)
#else
getWarnings :: String -> IO [String]
getWarnings file = runGhc (pure libdir) $ do
  setupDynFlags
  setupTargets

  parseWarnings

  where
    parseWarnings :: GhcMonad m => m [String]
    parseWarnings = do
      moduleGraph <- depanal [] True

      let moduleSummary =
            fromMaybe
              (error $ "Cannot find module summary for " ++ file ++ " in dependency graph")
              (find ((== file) . msHsFilePath) $ mgModSummaries moduleGraph)

      env          <- getSession
      dynFlags     <- getDynFlags
      parsedModule <- liftIO $ hscParse env moduleSummary

      let sDocContext = initSDocContext dynFlags defaultUserStyle

      traverse (render sDocContext) . bagToList . warnings Context{..} $ hpm_module parsedModule

    setupDynFlags :: GhcMonad m => m ()
    setupDynFlags = do
      dynFlags <- getSessionDynFlags
      void . setSessionDynFlags $ dynFlags { packageFlags = [packageFlag] }
      where
        packageFlag =
          ExposePackage
            "-package base"
            (PackageArg "base")
            (ModRenaming True [])

    setupTargets :: GhcMonad m => m ()
    setupTargets = do
      target <- guessTarget file Nothing
      setTargets [target]
      result <- load LoadAllTargets

      unless (succeeded result) $ error "Loading of targets failed"

    render :: GhcMonad m => SDocContext -> WarnMsg -> m String
    render sDocContext warning = do
      caretDiagnostic <- liftIO $
        getCaretDiagnostic
          (errMsgSeverity warning)
          (errMsgSpan warning)

      pure $ renderWithContext
        sDocContext
        (formatBulleted sDocContext (errMsgDiagnostic warning) $+$ caretDiagnostic)
#endif
