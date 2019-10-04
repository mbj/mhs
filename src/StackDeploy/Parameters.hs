module StackDeploy.Parameters
  ( Parameters
  , cfParameters
  , empty
  , expandTemplate
  , fromList
  , union
  )
where

import Control.Lens ((&), (?~), view)
import Data.HashMap.Strict (HashMap)
import Data.Set (Set)
import StackDeploy.Prelude hiding (empty)
import StackDeploy.Template

import qualified Control.Applicative              as Alternative
import qualified Data.Foldable                    as Foldable
import qualified Data.HashMap.Strict              as HashMap
import qualified Data.Set                         as Set
import qualified Network.AWS.CloudFormation.Types as CF
import qualified Stratosphere

newtype Parameters = Parameters (HashMap Text CF.Parameter)

empty :: Parameters
empty = Parameters HashMap.empty

fromList :: [(Text, CF.Parameter)] -> Parameters
fromList = Parameters . HashMap.fromList

cfParameters :: Parameters -> [CF.Parameter]
cfParameters (Parameters hash) = HashMap.elems hash

union :: Parameters -> Parameters -> Parameters
union (Parameters left) (Parameters right) =
  Parameters $ HashMap.union right left

expandTemplate :: Parameters -> Template -> Parameters
expandTemplate parameters@(Parameters hash) template
  = parameters `union` usePreviousParameters
  where
    usePreviousParameters :: Parameters
    usePreviousParameters
      =   Parameters
      .   HashMap.fromList
      $   mkPair
      <$> Foldable.toList missingParameterNames

    mkPair parameterName = (parameterName, mkUsePrevious parameterName)

    mkUsePrevious parameterName
      = CF.parameter
      & CF.pParameterKey     ?~ parameterName
      & CF.pUsePreviousValue ?~ True

    missingParameterNames :: Set Text
    missingParameterNames =
      Set.difference
        templateParameterNames
        givenParameterNames

    givenParameterNames :: Set Text
    givenParameterNames = Set.fromList $ HashMap.keys hash

    templateParameterNames :: Set Text
    templateParameterNames
      = Set.fromList $ view Stratosphere.parameterName <$> templateParameters

    templateParameters :: [Stratosphere.Parameter]
    templateParameters
      = maybe
          Alternative.empty
          Stratosphere.unParameters
      $ view Stratosphere.templateParameters (stratosphere template)
