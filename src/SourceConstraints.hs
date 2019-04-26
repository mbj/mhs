{-# LANGUAGE RankNTypes #-}

module SourceConstraints (Context(..), plugin, warnings) where

import Bag (emptyBag, unitBag, unionManyBags)
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
import Data.Generics.Aliases (ext2Q, extQ, mkQ)
import Data.Generics.Text (gshow)
import Data.List (intercalate, sort, sortBy, zip3)
import Data.Maybe (Maybe(Nothing), fromJust, maybe)
import Data.Ord (Ord(compare))
import Data.Semigroup ((<>))
import Data.String (String)
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
    , maybe emptyBag unitBag   $ locatedWarning context node
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

locatedWarning :: Data a => Context -> a -> Maybe ErrMsg
locatedWarning context = mkQ empty sortedImportStatement
  where
    sortedImportStatement :: HsModule GhcPs -> Maybe ErrMsg
    sortedImportStatement HsModule{..} = sortedLocated "import statement" context hsmodImports

unlocatedWarning :: Data a => Context -> a -> Maybe SDoc
unlocatedWarning context =
  mkQ empty requireDerivingStrategy
    `extQ` sortedIEThingWith context
    `extQ` sortedIEs context
    `extQ` sortedMultipleDeriving context

requireDerivingStrategy :: HsDerivingClause GhcPs -> Maybe SDoc
requireDerivingStrategy = \case
  HsDerivingClause{deriv_clause_strategy = Nothing} ->
    pure $ text "Missing deriving strategy"
  _ -> empty

sortedMultipleDeriving :: Context -> [LHsDerivingClause GhcPs] -> Maybe SDoc
sortedMultipleDeriving context clauses =
  if rendered /= expected
     then pure $ text . message $ intercalate ", " expected
     else empty

  where
    message :: String -> String
    message example = "Unsorted multiple deriving, expected: " <> example

    rendered = render context <$> clauses
    expected = sort rendered

data IEClass = Module String | Type String | Operator String | Function String
  deriving stock (Eq, Ord)

sortedIEs :: Context -> [LIE GhcPs] -> Maybe SDoc
sortedIEs context ies =
  if ies /= expected
    then pure . text . message $ intercalate ", " (render context <$> expected)
    else empty
  where
    message :: String -> String
    message example = "Unsorted import/export, expected: (" <> example <> ")"

    expected :: [LIE GhcPs]
    expected = sortBy (compare `on` ieClass . unLoc) ies

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

sortedIEThingWith :: Context -> IE GhcPs -> Maybe SDoc
sortedIEThingWith context = \case
  (IEThingWith _xIE _wrappedName _ieWildcard ieWith _ieFieldLabels) ->
    if rendered /= expected
       then pure $ text . message $ intercalate ", " expected
       else empty
    where
      message :: String -> String
      message example = "Unsorted import/export item with list, expected: (" <> example <> ")"

      rendered = render context <$> ieWith
      expected = sort rendered
  _ -> empty

render :: Outputable a => Context -> a -> String
render Context{..} outputable =
  renderWithStyle
    dynFlags
    (ppr outputable)
    (defaultUserStyle dynFlags)

sortedLocated
  :: forall a . Outputable a
  => String
  -> Context
  -> [Located a]
  -> Maybe ErrMsg
sortedLocated name context@Context{..} nodes = mkWarning <$> violation
  where
    mkWarning :: (String, String, Located a) -> ErrMsg
    mkWarning (_rendered, expected, node) =
      mkWarnMsg
        dynFlags
        (getLoc node)
        neverQualify
        (text $ "Unsorted " <> name <> ", expected: " <> expected)

    violation = find testViolation candidates

    testViolation :: (String, String, Located a) -> Bool
    testViolation (rendered, expected, _node) = rendered /= expected

    candidates :: [(String, String, Located a)]
    candidates =
      let rendered = render context <$> nodes
      in
        zip3 rendered (sort rendered) nodes
