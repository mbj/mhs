{-# LANGUAGE RankNTypes #-}

module SourceConstraints (Context(..), plugin, warnings) where

import Control.Applicative ((<$>), pure)
import Control.Monad
import Control.Monad.IO.Class
import Data.Attoparsec.Text
import Data.Bool
import Data.Char
import Data.Data
import Data.Either
import Data.Eq
import Data.Foldable
import Data.Function
import Data.Generics.Aliases
import Data.Generics.Text
import Data.List ((++))
import Data.Maybe
import Data.Ord
import Data.String (String)
import Data.Text (Text, pack)
import Data.Tuple
#if MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
import GHC.Data.Bag
import GHC.Driver.Plugins
import GHC.Driver.Session
import GHC.Driver.Types
import GHC.Hs
import GHC.Types.SrcLoc
import GHC.Unit.Module.Location
import GHC.Utils.Error
import GHC.Utils.Outputable
#else
import Bag
import GHC.Hs
import DynFlags
import HscTypes
import Module hiding (Module)
import Outputable hiding ((<>), empty)
import Plugins
import SrcLoc
import ErrUtils
#endif
import Prelude(error)
import SourceConstraints.LocalModule
import System.FilePath.Posix

import qualified Data.List as List

data Context = Context
  { dynFlags     :: DynFlags
  , localModules :: [LocalModule]
#if MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
  , sDocContext  :: SDocContext
#endif
  }

#if MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
type HsModule' = HsModule
#else
type HsModule' = HsModule GhcPs
#endif

plugin :: Plugin
plugin =
  defaultPlugin
  { parsedResultAction = run
  , pluginRecompile    = purePlugin
  }

run
  :: [CommandLineOption]
  -> ModSummary
  -> HsParsedModule
  -> Hsc HsParsedModule
run options summary parsedModule = do
  dynFlags <- getDynFlags
  runSourceConstraints dynFlags options summary parsedModule

runSourceConstraints
  :: DynFlags
  -> [CommandLineOption]
  -> ModSummary
  -> HsParsedModule
  -> Hsc HsParsedModule
runSourceConstraints dynFlags options ModSummary{ms_location = ModLocation{..}} parsedModule = do
  localModules <- mapM parseLocalModule (pack <$> options)

#if MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
  let sDocContext = initSDocContext dynFlags defaultUserStyle
#endif

  when (allowLocation ml_hs_file) . emitWarnings $ warnings Context{..} (hpm_module parsedModule)
  pure parsedModule
  where
    allowLocation = maybe False (notElem "autogen/" . splitPath)

    emitWarnings :: Bag WarnMsg -> Hsc ()
    emitWarnings  = liftIO . printOrThrowWarnings dynFlags

    parseLocalModule :: Text -> Hsc LocalModule
    parseLocalModule
      = either localParseFailure pure . parseOnly localModuleParser

    localParseFailure :: String -> Hsc a
    localParseFailure =
      throwOneError . mkPlainErrMsg dynFlags noSrcSpan . text

-- | Find warnings for node
warnings
  :: (Data a, Data b, Typeable a)
  => Context
  -> GenLocated a b
  -> WarningMessages
warnings context current@(L _sourceSpan node) =
  unionManyBags
    [ locatedWarnings context current
    , locatedWarnings context node
    , descend node
    ]
  where
    descend :: Data a => a -> WarningMessages
    descend =
      unionManyBags . gmapQ
        (descend `ext2Q` warnings context)

data IEClass = Module String | Type String | Operator String | Function String
  deriving stock (Eq, Ord)

locatedWarnings :: Data a => Context -> a -> WarningMessages
locatedWarnings context@Context{..} node =
  singleWarnings `unionBags` mkQ emptyBag absentImportDeclList node
  where
    singleWarnings = listToBag $ catMaybes
      [ mkQ Nothing requireDerivingStrategy node
      , mkQ Nothing sortedImportStatement   node
      , mkQ Nothing sortedIEThingWith       node
      , mkQ Nothing sortedIEs               node
      , mkQ Nothing sortedMultipleDeriving  node
      ]

    absentImportDeclList :: HsModule' -> WarningMessages
    absentImportDeclList HsModule{..} =
      listToBag $ catMaybes (absentList <$> candidates)
      where
        absentList :: LImportDecl GhcPs -> Maybe ErrMsg
        absentList = \case
          (L _loc ImportDecl { ideclHiding = Just (False, L loc list) }) ->
            testList loc list
          _  -> Nothing

        testList :: SrcSpan -> [LIE GhcPs] -> Maybe ErrMsg
        testList srcSpan = \case
          [] -> Nothing
          _  -> pure $ notEmpty srcSpan

        notEmpty :: SrcSpan -> ErrMsg
        notEmpty src =
          mkWarnMsg
            dynFlags
            src
            neverQualify
            (text "Present import list for local module")

        candidates :: [LImportDecl GhcPs]
        candidates = List.filter isCandidate hsmodImports

        isCandidate :: LImportDecl GhcPs -> Bool
        isCandidate (L _ ImportDecl{ideclName = L _ moduleName})
          = any (`isLocalModule` moduleName) localModules

#if !MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
        isCandidate _  = False
#endif

    requireDerivingStrategy :: LHsDerivingClause GhcPs -> Maybe ErrMsg
    requireDerivingStrategy = \case
      (L src HsDerivingClause{deriv_clause_strategy = Nothing}) ->
        pure $ mkWarnMsg dynFlags src neverQualify (text "Missing deriving strategy")
      _ -> Nothing

    sortedImportStatement :: HsModule' -> Maybe ErrMsg
    sortedImportStatement HsModule{..} =
      sortedLocated
        "import statement"
        context
        (render context)
        hsmodImports

    sortedMultipleDeriving :: [LHsDerivingClause GhcPs] -> Maybe ErrMsg
    sortedMultipleDeriving =
      sortedLocated
        "deriving clauses"
        context
        (render context)

    sortedIEs :: [LIE GhcPs] -> Maybe ErrMsg
    sortedIEs =
      sortedLocated
        "import/export declaration"
        context
        ieClass

    sortedIEThingWith :: IE GhcPs -> Maybe ErrMsg
    sortedIEThingWith = \case
      (IEThingWith _xIE _name _ieWildcard ieWith _ieFieldLabels) ->
        sortedLocated
          "import/export item with list"
          context
          (render context)
          ieWith
      _ -> Nothing

    classify str@('(':_) = Function str
    classify str@(x:_)   = if isUpper x then Type str else Function str
    classify []          = error "Parser error"

    ieClass :: IE GhcPs -> IEClass
    ieClass = \case
      (IEVar _xIE name)            -> mkClass classify name
      (IEThingAbs _xIE name)       -> mkClass Type name
      (IEThingAll _xIE name)       -> mkClass Type name
      (IEModuleContents _xIE name) -> mkClass Module name
      (IEThingWith _xIE name _ieWildcard _ieWith _ieFieldLabels) ->
        mkClass Type name
      ie -> error $ "Unsupported: " ++ gshow ie

    mkClass :: (Outputable a, Outputable b) => (String -> IEClass) -> GenLocated a b -> IEClass
    mkClass constructor name = constructor $ render context name

render :: Outputable a => Context -> a -> String
render Context{..} outputable =
#if MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
  renderWithStyle sDocContext $ ppr outputable
#else
  renderWithStyle
    dynFlags
    (ppr outputable)
    (defaultUserStyle dynFlags)
#endif

sortedLocated
  :: forall a b . (Eq b, Ord b, Outputable a)
  => String
  -> Context
  -> (a -> b)
  -> [Located a]
  -> Maybe ErrMsg
sortedLocated name context@Context{..} ordering nodes =
  mkWarning <$> find isViolation candidates
  where
    mkWarning :: ((Located a, b), (Located a, b)) -> ErrMsg
    mkWarning ((actualNode, _item), (expectedNode, _expected)) =
      mkWarnMsg
        dynFlags
        (getLoc actualNode)
        neverQualify
        (text $ "Unsorted " ++ name ++ ", expected: " ++ render context expectedNode)

    isViolation :: ((Located a, b), (Located a, b)) -> Bool
    isViolation ((_actualNode, item), (_expectedNode, expected)) =
      item /= expected

    candidates :: [((Located a, b), (Located a, b))]
    candidates =
      let items    = (\node -> (node, ordering $ unLoc node)) <$> nodes
          expected = List.sortBy (compare `on` snd) items
      in
        List.zip items expected
