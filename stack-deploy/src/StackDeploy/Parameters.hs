module StackDeploy.Parameters
  ( Parameter(..)
  , ParameterName(..)
  , ParameterValue(..)
  , Parameters
  , cfParameters
  , empty
  , expandTemplate
  , fromStratosphereParameter
  , union
  )
where

import Control.Lens ((?~))
import Data.Map.Strict (Map)
import Data.Set (Set)
import StackDeploy.Prelude hiding (empty)
import StackDeploy.Template

import qualified Amazonka.CloudFormation.Types as CF
import qualified Control.Applicative           as Alternative
import qualified Data.Foldable                 as Foldable
import qualified Data.List                     as List
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import qualified Stratosphere

newtype ParameterName = ParameterName Text
  deriving (Conversion Text) via Text
  deriving stock (Eq, Ord)

newtype ParameterValue = ParameterValue Text
  deriving (Conversion Text) via Text
  deriving stock Eq

data Parameter
  = Parameter ParameterName ParameterValue
  | ParameterUsePrevious ParameterName

newtype Parameters = Parameters (Map ParameterName Parameter)

parameterName :: Parameter -> ParameterName
parameterName = \case
  (Parameter name _value)     -> name
  (ParameterUsePrevious name) -> name

empty :: Parameters
empty = Parameters []

fromStratosphereParameter
  :: Stratosphere.Parameter
  -> ParameterValue
  -> Parameter
fromStratosphereParameter = Parameter . ParameterName . (.name)

instance IsList Parameters where
  type Item Parameters = Parameter

  fromList parameters =
    Parameters . Map.fromList $ pairs
    where
      pairs :: [(ParameterName, Parameter)]
      pairs = mkPair <$> parameters

      mkPair :: Parameter -> (ParameterName, Parameter)
      mkPair parameter = (parameterName parameter, parameter)

  toList (Parameters map) = List.sortOn parameterName $ Map.elems map

cfParameters :: Parameters -> [CF.Parameter]
cfParameters parameters = mkCFParameter <$> toList parameters

mkCFParameter :: Parameter -> CF.Parameter
mkCFParameter = \case
  Parameter name value ->
    CF.newParameter
      & CF.parameter_parameterKey   ?~ toText name
      & CF.parameter_parameterValue ?~ toText value
  ParameterUsePrevious name ->
    CF.newParameter
      & CF.parameter_parameterKey     ?~ toText name
      & CF.parameter_usePreviousValue ?~ True

union :: Parameters -> Parameters -> Parameters
union (Parameters left) (Parameters right) =
  Parameters $ Map.union right left

expandTemplate :: Parameters -> Template -> Parameters
expandTemplate parameters@(Parameters hash) template
  = parameters `union` usePreviousParameters
  where
    usePreviousParameters :: Parameters
    usePreviousParameters
      =   Parameters
      .   Map.fromList
      $   mkPair
      <$> Foldable.toList missingParameterNames

    mkPair name = (name, ParameterUsePrevious name)

    missingParameterNames :: Set ParameterName
    missingParameterNames =
      Set.difference
        templateParameterNames
        givenParameterNames

    givenParameterNames :: Set ParameterName
    givenParameterNames = Set.fromList $ Map.keys hash

    templateParameterNames :: Set ParameterName
    templateParameterNames
      = Set.fromList
      $ ParameterName . (.name) <$> templateParameters

    templateParameters :: [Stratosphere.Parameter]
    templateParameters
      = maybe Alternative.empty (.parameterList)
      $ template.stratosphere.parameters
