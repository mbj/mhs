{-# LANGUAGE RankNTypes #-}

module SourceConstraints (plugin) where

import Bag (emptyBag, unitBag, unionBags, unionManyBags)
import Control.Applicative (Alternative(empty), Applicative(pure))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Bool (Bool(False), not)
import Data.Char (isUpper)
import Data.Data (Data, Typeable, cast, gmapQ)
import Data.Eq (Eq((/=)))
import Data.Foldable (elem)
import Data.Function (($), (.), on)
import Data.Functor ((<$>))
import Data.Generics.Aliases (ext2Q, extQ, mkQ)
import Data.Generics.Text (gshow)
import Data.List (intercalate, sortBy)
import Data.Maybe (Maybe(Just, Nothing), fromJust, maybe)
import Data.Ord (Ord(compare))
import Data.Semigroup ((<>))
import Data.String (String)
import DynFlags (DynFlags, getDynFlags)
import ErrUtils (WarningMessages, mkWarnMsg)
import HsDecls (HsDerivingClause(HsDerivingClause, deriv_clause_strategy))
import HsExtension (GhcPs)
import Module(ModLocation(ModLocation, ml_hs_file))
import SrcLoc (GenLocated(L), unLoc)
import System.FilePath.Posix (splitPath)

import Prelude(error)

import HsSyn
  ( IE
    ( IEModuleContents
    , IEThingAbs
    , IEThingAll
    , IEThingWith
    , IEVar
    )
  , LIE
  )

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

import HscTypes
  ( HsParsedModule(HsParsedModule, hpm_module)
  , Hsc
  , ModSummary(ModSummary, ms_location)
  , printOrThrowWarnings
  )

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

  when (allowLocation ml_hs_file) $ do
    liftIO
      . printOrThrowWarnings dynFlags
      $ warnings dynFlags parsedModule

  pure parsedModule
  where
    allowLocation (Just path) = not $ elem ".stack-work/" $ splitPath path
    allowLocation Nothing     = False

warnings :: DynFlags -> HsParsedModule -> WarningMessages
warnings dynFlags HsParsedModule{..} = locatedWarnings dynFlags hpm_module

findWarnings :: Data a => DynFlags -> a -> WarningMessages
findWarnings dynFlags =
  unionManyBags . gmapQ
    (findWarnings dynFlags `ext2Q` locatedWarnings dynFlags)

-- | Find warnings directly underneath a located node
locatedWarnings :: (Data b, Typeable a)
                => DynFlags
                -> GenLocated a b
                -> WarningMessages
locatedWarnings dynFlags (L sourceSpan node) =
  unionBags
    (maybe emptyBag mkWarning $ unlocatedWarning dynFlags node)
    (findWarnings dynFlags node)
  where
    mkWarning =
      unitBag . mkWarnMsg
        dynFlags
        (fromJust $ cast sourceSpan)
        neverQualify

unlocatedWarning :: Data a => DynFlags -> a -> Maybe SDoc
unlocatedWarning dynFlags = mkQ empty requireDerivingStrategy `extQ` (sortedIEs dynFlags)

requireDerivingStrategy :: HsDerivingClause GhcPs -> Maybe SDoc
requireDerivingStrategy HsDerivingClause{deriv_clause_strategy = Nothing}
  = pure $ text "Missing deriving strategy"
requireDerivingStrategy _
  = empty

data IEClass = Module String | Type String | Operator String | Function String
  deriving stock (Eq, Ord)

sortedIEs :: DynFlags -> [LIE GhcPs] -> Maybe SDoc
sortedIEs dynFlags ies =
  if ies /= expected
    then pure . text . message $ intercalate ", " (render <$> expected)
    else empty
  where
    message :: String -> String
    message example = "Unsorted import/export expected: (" <> example <> ")"

    expected :: [LIE GhcPs]
    expected = sortBy (compare `on` ieClass . unLoc) ies

    classify str@('(':_) = Function str
    classify str@(x:_)   = if isUpper x then Type str else Function str
    classify []          = error "Parser error"

    ieClass :: IE GhcPs -> IEClass
    ieClass (IEVar _xIE wrappedName) =
      classify . render $ unLoc wrappedName
    ieClass (IEThingAbs _xIE wrappedName) =
      Type . render $ unLoc wrappedName
    ieClass (IEThingAll _xIE wrappedName) =
      Type . render $ unLoc wrappedName
    ieClass (IEThingWith _xIE wrappedName _ieWildcard _ieWith _ieFieldLabels) =
      Type . render $ unLoc wrappedName
    ieClass (IEModuleContents _xIE moduleName) =
      Module . render $ unLoc moduleName
    ieClass ie = error $ "Unsupported: " <> gshow ie

    render :: Outputable a => a -> String
    render outputable =
      renderWithStyle
        dynFlags
        (ppr outputable)
        (defaultUserStyle dynFlags)
