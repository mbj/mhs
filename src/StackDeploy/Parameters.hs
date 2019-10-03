module StackDeploy.Parameters (addNoopParams) where

import Control.Lens ((&), (?~), view)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Text (Text)
import StackDeploy.Prelude
import StackDeploy.Types

import qualified Data.Foldable                    as Foldable
import qualified Data.Set                         as Set
import qualified Network.AWS.CloudFormation.Types as CF
import qualified Stratosphere

addNoopParams :: InstanceSpec -> Stratosphere.Template -> InstanceSpec
addNoopParams instanceSpec@InstanceSpec{..} template = instanceSpec
  { parameters = parameters <> usePreviousParameters }
  where
    usePreviousParameters :: [CF.Parameter]
    usePreviousParameters = mkUsePrevious <$> Foldable.toList missingParameterNames

    mkUsePrevious name
      = CF.parameter
      & CF.pParameterKey     ?~ name
      & CF.pUsePreviousValue ?~ True

    missingParameterNames :: Set Text
    missingParameterNames =
      Set.difference
        templateParameterNames
        instanceSpecParameterNames

    instanceSpecParameterNames :: Set Text
    instanceSpecParameterNames
      = Set.fromList
      . catMaybes
      $ view CF.pParameterKey <$> parameters

    templateParameterNames :: Set Text
    templateParameterNames
      = Set.fromList $ view Stratosphere.parameterName <$> templateParameters

    templateParameters :: [Stratosphere.Parameter]
    templateParameters
      = maybe
          empty
          Stratosphere.unParameters
      $ view Stratosphere.templateParameters template
