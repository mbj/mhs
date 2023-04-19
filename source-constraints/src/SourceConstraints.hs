{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns     #-}

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
import GHC.Data.Bag
import GHC.Driver.Env.Types
import GHC.Driver.Errors
import GHC.Driver.Plugins
import GHC.Driver.Session
import GHC.Hs
import GHC.Types.Error
import GHC.Types.SourceError
import GHC.Types.SrcLoc
import GHC.Unit.Module.Location
import GHC.Unit.Module.ModSummary
import GHC.Utils.Logger
import GHC.Utils.Outputable
import Prelude(error)
import SourceConstraints.LocalModule
import System.FilePath.Posix

import qualified Data.List as List

#if MIN_VERSION_GLASGOW_HASKELL(9,4,0,0)
import GHC.Driver.Config.Diagnostic
import GHC.Driver.Errors.Types
import GHC.Utils.Error
#endif

data Context = Context
  { localModules :: [LocalModule]
  , sDocContext  :: SDocContext
  }

plugin :: Plugin
plugin =
  defaultPlugin
  { parsedResultAction = run
  , pluginRecompile    = purePlugin
  }

#if MIN_VERSION_GLASGOW_HASKELL(9,4,0,0)
type Message = MsgEnvelope GhcMessage

run
  :: [CommandLineOption]
  -> ModSummary
  -> ParsedResult
  -> Hsc ParsedResult
run options ModSummary{ms_location = ModLocation{..}} parsedResult@ParsedResult{..} = do
  dynFlags <- getDynFlags
  logger   <- getLogger

  localModules <- mapM parseLocalModule (pack <$> options)

  let sDocContext = initSDocContext dynFlags defaultUserStyle

  when (allowLocation ml_hs_file) . (emitWarnings logger dynFlags) $
    warnings Context{..} (hpm_module parsedResultModule)

  pure parsedResult
  where
    allowLocation = maybe False (notElem "autogen/" . splitPath)

    emitWarnings :: Logger -> DynFlags -> WarningMessages -> Hsc ()
    emitWarnings logger dynFlags = liftIO . printOrThrowDiagnostics logger (initDiagOpts dynFlags)

    parseLocalModule :: Text -> Hsc LocalModule
    parseLocalModule
      = either localParseFailure pure . parseOnly localModuleParser

    localParseFailure :: String -> Hsc a
    localParseFailure
      = throwOneError
      . mkPlainErrorMsgEnvelope noSrcSpan
      . ghcUnknownMessage
      . mkPlainError []
      . text

-- | Find warnings for node
warnings
  :: (Data a, Data b)
  => Context
  -> GenLocated a b
  -> WarningMessages
warnings context current@(L _sourceSpan node) =
  unionManyMessages
    [ locatedWarnings context current
    , locatedWarnings context node
    , descend node
    ]
  where
    descend :: Data a => a -> WarningMessages
    descend =
      unionManyMessages . gmapQ
        (descend `ext2Q` warnings context)

locatedWarnings :: Data a => Context -> a -> WarningMessages
locatedWarnings context@Context{..} node =
  singleWarnings `unionMessages` mkQ emptyMessages absentImportDeclList node
  where
    singleWarnings :: WarningMessages
    singleWarnings = mkMessages . listToBag $ catMaybes
      [ mkQ Nothing sortedImportStatement   node
      , mkQ Nothing sortedIEThingWith       node
      , mkQ Nothing sortedIEs               node
      , mkQ Nothing sortedMultipleDeriving  node
      ]

    absentImportDeclList :: HsModule -> WarningMessages
    absentImportDeclList HsModule{..} =
      mkMessages . listToBag $ catMaybes (absentList <$> candidates)
      where
        absentList :: LImportDecl GhcPs -> Maybe Message
        absentList = \case
          (L _loc ImportDecl { ideclHiding = Just (False, reLoc -> (L src (_:_))) }) ->
            pure $ mkWarnMsg
              src
              neverQualify
              (text "Present import list for local module")
          _  -> Nothing

        candidates :: [LImportDecl GhcPs]
        candidates = List.filter isCandidate hsmodImports

        isCandidate :: LImportDecl GhcPs -> Bool
        isCandidate (L _ ImportDecl{ideclName = L _ moduleName})
          = any (`isLocalModule` moduleName) localModules

    sortedImportStatement HsModule{..} =
      sortedLocated
        "import statement"
        context
        (render context)
        (reLoc <$> hsmodImports)

    sortedMultipleDeriving :: HsDecl GhcPs -> Maybe Message
    sortedMultipleDeriving = \case
      (TyClD _xIE DataDecl{tcdDataDefn = HsDataDefn {..}}) ->
        sortedLocated "deriving clauses" context (render context) (mkLocated <$> dd_derivs)
      _ -> Nothing
      where
        mkLocated
          :: GenLocated (SrcAnn NoEpAnns) (HsDerivingClause GhcPs)
          -> Located (HsDerivingClause GhcPs)
        mkLocated value = L (locA $ getLoc value) (unLoc value)

    sortedIEs :: [LIE GhcPs] -> Maybe Message
    sortedIEs lie =
      sortedLocated
        "import/export declaration"
        context
        ieClass
        (reLoc <$> lie)

    sortedIEThingWith :: IE GhcPs -> Maybe Message
    sortedIEThingWith =
      \case
        (IEThingWith _xIE _name _ieWildcard ieWith) ->
          sortedLocated
            "import/export item with list"
            context
            (render context)
            (reLoc <$> ieWith)
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
      (IEThingWith _xIE name _ieWildcard _ieFieldLabels) ->
        mkClass Type name
      ie -> error $ "Unsupported: " ++ gshow ie
      where
        mkClass
          :: Outputable (GenLocated a b)
          => (String -> IEClass)
          -> GenLocated a b
          -> IEClass
        mkClass constructor name = constructor $ render context name

mkWarnMsg :: SrcSpan -> PrintUnqualified -> SDoc -> Message
mkWarnMsg srcSpan printUnqualified sdoc
  = MsgEnvelope
  { errMsgContext    = printUnqualified
  , errMsgDiagnostic = GhcUnknownMessage $ mkPlainDiagnostic WarningWithoutFlag [] sdoc
  , errMsgSeverity   = SevWarning
  , errMsgSpan       = srcSpan
  }
#else

type Message = MsgEnvelope DecoratedSDoc

run
  :: [CommandLineOption]
  -> ModSummary
  -> HsParsedModule
  -> Hsc HsParsedModule
run options ModSummary{ms_location = ModLocation{..}} parsedModule = do
  dynFlags <- getDynFlags
  logger   <- getLogger

  localModules <- mapM parseLocalModule (pack <$> options)

  let sDocContext = initSDocContext dynFlags defaultUserStyle

  when (allowLocation ml_hs_file) . (emitWarnings logger dynFlags) $
    warnings Context{..} (hpm_module parsedModule)

  pure parsedModule
  where
    allowLocation = maybe False (notElem "autogen/" . splitPath)

    emitWarnings :: Logger -> DynFlags -> Bag WarnMsg -> Hsc ()
    emitWarnings logger dynFlags = liftIO . printOrThrowWarnings logger dynFlags

    parseLocalModule :: Text -> Hsc LocalModule
    parseLocalModule
      = either localParseFailure pure . parseOnly localModuleParser

    localParseFailure :: String -> Hsc a
    localParseFailure = throwOneError . mkPlainMsgEnvelope noSrcSpan . text

-- | Find warnings for node
warnings
  :: (Data a, Data b)
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

locatedWarnings :: Data a => Context -> a -> WarningMessages
locatedWarnings context@Context{..} node =
  singleWarnings `unionBags` mkQ emptyBag absentImportDeclList node
  where
    singleWarnings = listToBag $ catMaybes
      [ mkQ Nothing sortedImportStatement   node
      , mkQ Nothing sortedIEThingWith       node
      , mkQ Nothing sortedIEs               node
      , mkQ Nothing sortedMultipleDeriving  node
      ]

    absentImportDeclList :: HsModule -> WarningMessages
    absentImportDeclList HsModule{..} =
      listToBag $ catMaybes (absentList <$> candidates)
      where
        absentList :: LImportDecl GhcPs -> Maybe Message
        absentList = \case
          (L _loc ImportDecl { ideclHiding = Just (False, reLoc -> (L src (_:_))) }) ->
            pure $ mkWarnMsg
              src
              neverQualify
              (text "Present import list for local module")
          _  -> Nothing

        candidates :: [LImportDecl GhcPs]
        candidates = List.filter isCandidate hsmodImports

        isCandidate :: LImportDecl GhcPs -> Bool
        isCandidate (L _ ImportDecl{ideclName = L _ moduleName})
          = any (`isLocalModule` moduleName) localModules

    sortedImportStatement HsModule{..} =
      sortedLocated
        "import statement"
        context
        (render context)
        (reLoc <$> hsmodImports)

    sortedMultipleDeriving :: HsDecl GhcPs -> Maybe Message
    sortedMultipleDeriving = \case
      (TyClD _xIE DataDecl{tcdDataDefn = HsDataDefn {..}}) ->
        sortedLocated "deriving clauses" context (render context) dd_derivs
      _ -> Nothing

    sortedIEs :: [LIE GhcPs] -> Maybe Message
    sortedIEs lie =
      sortedLocated
        "import/export declaration"
        context
        ieClass
        (reLoc <$> lie)

    sortedIEThingWith :: IE GhcPs -> Maybe Message
    sortedIEThingWith =
      \case
        (IEThingWith _xIE _name _ieWildcard ieWith) ->
          sortedLocated
            "import/export item with list"
            context
            (render context)
            (reLoc <$> ieWith)
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
      (IEThingWith _xIE name _ieWildcard _ieFieldLabels) ->
        mkClass Type name
      ie -> error $ "Unsupported: " ++ gshow ie
      where
        mkClass
          :: Outputable (GenLocated a b)
          => (String -> IEClass)
          -> GenLocated a b
          -> IEClass
        mkClass constructor name = constructor $ render context name
#endif

data IEClass = Module String | Type String | Operator String | Function String
  deriving stock (Eq, Ord)

render :: Outputable a => Context -> a -> String
render Context{..} outputable = renderWithContext sDocContext $ ppr outputable

sortedLocated
  :: forall a b . (Ord b, Outputable a)
  => String
  -> Context
  -> (a -> b)
  -> [Located a]
  -> Maybe Message
sortedLocated name context ordering nodes =
  mkWarning <$> find isViolation candidates
  where
    mkWarning :: ((Located a, b), (Located a, b)) -> Message
    mkWarning ((actualNode, _item), (expectedNode, _expected)) =
      mkWarnMsg
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
