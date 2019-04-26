{-# LANGUAGE RankNTypes #-}

module SourceConstraints (Context(..), plugin, warnings) where

import Bag (emptyBag, listToBag, unitBag, unionManyBags)
import Control.Applicative (Alternative(empty), Applicative(pure))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Bool (Bool(False), not)
import Data.Char (isUpper)
import Data.Data (Data, Typeable, cast, gmapQ)
import Data.Eq (Eq((/=)))
import Data.Foldable (Foldable(elem), find)
import Data.Function (($), (.), on)
import Data.Functor ((<$>))
import Data.Generics.Aliases (ext2Q, mkQ)
import Data.Generics.Text (gshow)
import Data.List (sortBy, zip)
import Data.Maybe (Maybe(Nothing), catMaybes, fromJust, maybe)
import Data.Ord (Ord(compare))
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Tuple (snd)
import DynFlags (DynFlags, getDynFlags)
import ErrUtils (ErrMsg, WarningMessages, mkWarnMsg)
import HsDecls
  ( HsDerivingClause
    ( HsDerivingClause
    , deriv_clause_strategy
    )
  , LHsDerivingClause
  )
import HsExtension (GhcPs)
import HsSyn
  ( IE
    ( IEModuleContents
    , IEThingAbs
    , IEThingAll
    , IEThingWith
    , IEVar
    )
  , HsModule
    ( HsModule
    , hsmodImports
    )
  , LIE
  )
import HscTypes
  ( HsParsedModule(hpm_module)
  , Hsc
  , ModSummary(ModSummary, ms_location)
  , printOrThrowWarnings
  )
import Module(ModLocation(ModLocation, ml_hs_file))
import Outputable
  ( Outputable
  , SDoc
  , defaultUserStyle
  , neverQualify
  , ppr
  , renderWithStyle
  , text
  )
import Plugins
  ( CommandLineOption
  , Plugin
  , defaultPlugin
  , parsedResultAction
  , pluginRecompile
  , purePlugin
  )
import Prelude(error)
import SrcLoc (GenLocated(L), Located, getLoc, unLoc)
import System.FilePath.Posix (splitPath)

newtype Context = Context
  { dynFlags :: DynFlags
  }

plugin :: Plugin
plugin =
  defaultPlugin
  { parsedResultAction = runSourceConstraints
  , pluginRecompile    = purePlugin
  }

runSourceConstraints :: [CommandLineOption]
                     -> ModSummary
                     -> HsParsedModule
                     -> Hsc HsParsedModule
runSourceConstraints _options ModSummary{ms_location = ModLocation{..}} parsedModule = do
  dynFlags <- getDynFlags

  when (allowLocation ml_hs_file) $
    liftIO
      . printOrThrowWarnings dynFlags
      $ warnings Context{..} (hpm_module parsedModule)

  pure parsedModule
  where
    allowLocation = maybe False (not . elem ".stack-work/" . splitPath)

-- | Find warnings for node
warnings
  :: (Data a, Data b, Typeable a)
  => Context
  -> GenLocated a b
  -> WarningMessages
warnings context@Context{..} (L sourceSpan node) =
  unionManyBags
    [ maybe emptyBag mkWarning $ unlocatedWarning context node
    , locatedWarnings context node
    , descend node
    ]
  where
    mkWarning =
      unitBag . mkWarnMsg
        dynFlags
        (fromJust $ cast sourceSpan)
        neverQualify

    descend :: Data a => a -> WarningMessages
    descend =
      unionManyBags . gmapQ
        (descend `ext2Q` warnings context)

data IEClass = Module String | Type String | Operator String | Function String
  deriving stock (Eq, Ord)

locatedWarnings :: Data a => Context -> a -> WarningMessages
locatedWarnings context node = listToBag $ catMaybes
  [ mkQ empty sortedImportStatement  node
  , mkQ empty sortedIEThingWith      node
  , mkQ empty sortedIEs              node
  , mkQ empty sortedMultipleDeriving node
  ]
  where
    sortedImportStatement :: HsModule GhcPs -> Maybe ErrMsg
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
      _ -> empty

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
      ie -> error $ "Unsupported: " <> gshow ie

    mkClass :: Outputable b => (String -> IEClass) -> GenLocated a b -> IEClass
    mkClass constructor name = constructor . render context $ unLoc name

unlocatedWarning :: Data a => Context -> a -> Maybe SDoc
unlocatedWarning _context = mkQ empty requireDerivingStrategy

requireDerivingStrategy :: HsDerivingClause GhcPs -> Maybe SDoc
requireDerivingStrategy = \case
  HsDerivingClause{deriv_clause_strategy = Nothing} ->
    pure $ text "Missing deriving strategy"
  _ -> empty

render :: Outputable a => Context -> a -> String
render Context{..} outputable =
  renderWithStyle
    dynFlags
    (ppr outputable)
    (defaultUserStyle dynFlags)

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
        (text $ "Unsorted " <> name <> ", expected: " <> render context expectedNode)

    isViolation :: ((Located a, b), (Located a, b)) -> Bool
    isViolation ((_actualNode, item), (_expectedNode, expected)) =
      item /= expected

    candidates :: [((Located a, b), (Located a, b))]
    candidates =
      let items    = (\node -> (node, ordering $ unLoc node)) <$> nodes
          expected = sortBy (compare `on` snd) items
      in
        zip items expected
