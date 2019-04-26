{-# LANGUAGE RankNTypes #-}

module SourceConstraints (Context(..), plugin, warnings) where

import Bag
import Control.Applicative
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
import Data.List
import Data.Maybe
import Data.Ord
import Data.Semigroup
import Data.String (String)
import Data.Tuple
import DynFlags
import ErrUtils
import HsDecls
import HsExtension
import HsSyn
import HscTypes
import Module hiding (Module)
import Outputable hiding ((<>), empty)
import Plugins
import Prelude(error)
import SrcLoc
import System.FilePath.Posix

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
    allowLocation = maybe False (notElem ".stack-work/" . splitPath)

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
