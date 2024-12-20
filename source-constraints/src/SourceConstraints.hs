{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module SourceConstraints (Context(..), plugin, warnings) where

import Control.Applicative ((<$>), pure)
import Control.Monad
import Control.Monad.IO.Class
import Data.Bool
import Data.Char
import Data.Data
import Data.Eq
import Data.Foldable
import Data.Function
import Data.Generics.Aliases
import Data.Generics.Text
import Data.List ((++))
import Data.Maybe
import Data.Ord
import Data.String (String)
import Data.Tuple
import GHC.Data.Bag
import GHC.Driver.Config.Diagnostic
import GHC.Driver.Env.Types
import GHC.Driver.Errors
import GHC.Driver.Errors.Types
import GHC.Driver.Plugins
import GHC.Driver.Session
import GHC.Hs
import GHC.Types.Error
import GHC.Types.SrcLoc
import GHC.Unit.Module.Location
import GHC.Unit.Module.ModSummary
import GHC.Utils.Error
import GHC.Utils.Logger
import GHC.Utils.Outputable
import Prelude(error)
import System.FilePath.Posix

import qualified Data.List as List

plugin :: Plugin
plugin =
  defaultPlugin
  { parsedResultAction = run
  , pluginRecompile    = purePlugin
  }

type Message = MsgEnvelope GhcMessage

data Context = Context
  { sDocContext  :: SDocContext
  , diagOpts     :: DiagOpts
  }

run
  :: [CommandLineOption]
  -> ModSummary
  -> ParsedResult
  -> Hsc ParsedResult
run _options ModSummary{ms_location = ModLocation{..}} parsedResult@ParsedResult{..} = do
  dynFlags <- getDynFlags
  logger   <- getLogger

  let sDocContext = initSDocContext dynFlags defaultUserStyle
      diagOpts    = initDiagOpts dynFlags

#if MIN_VERSION_base(4,18,0)
  when (allowLocation ml_hs_file) . emitWarnings diagOpts logger (defaultDiagnosticOpts @GhcMessage) $
    warnings Context{..} (hpm_module parsedResultModule)
#else
  when (allowLocation ml_hs_file) . emitWarnings diagOpts logger $
    warnings Context{..} (hpm_module parsedResultModule)
#endif

  pure parsedResult
  where
    allowLocation = maybe False (notElem "autogen/" . splitPath)

#if MIN_VERSION_base(4,18,0)
    emitWarnings :: DiagOpts -> Logger -> GhcMessageOpts -> WarningMessages -> Hsc ()
    emitWarnings diagOpts logger ghcMessageOpts
      = liftIO . printOrThrowDiagnostics logger ghcMessageOpts diagOpts
#else
    emitWarnings :: DiagOpts -> Logger -> WarningMessages -> Hsc ()
    emitWarnings diagOpts logger
      = liftIO . printOrThrowDiagnostics logger diagOpts
#endif

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
locatedWarnings context node
  = mkMessages . listToBag $ catMaybes
  [ mkQ Nothing sortedImportStatement   node
  , mkQ Nothing sortedIEThingWith       node
  , mkQ Nothing sortedIEs               node
  , mkQ Nothing sortedMultipleDeriving  node
  ]
  where
#if MIN_VERSION_base(4,18,0)
    sortedImportStatement :: HsModule GhcPs -> Maybe Message
#else
    sortedImportStatement :: HsModule -> Maybe Message
#endif
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

#if MIN_VERSION_base(4,18,0)
mkWarnMsg :: DiagOpts -> SrcSpan -> NamePprCtx -> SDoc -> Message
#else
mkWarnMsg :: DiagOpts -> SrcSpan -> PrintUnqualified -> SDoc -> Message
#endif
mkWarnMsg diagOpts srcSpan printUnqualified sdoc
  = MsgEnvelope
  { errMsgContext    = printUnqualified
#if MIN_VERSION_base(4,19,0)
  , errMsgReason     = ResolvedDiagnosticReason WarningWithoutFlag
#endif
  , errMsgDiagnostic = ghcUnknownMessage $ mkPlainDiagnostic WarningWithoutFlag [] sdoc
  , errMsgSeverity   = (diagReasonSeverity diagOpts WarningWithoutFlag)
  , errMsgSpan       = srcSpan
  }

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
        (diagOpts context)
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
