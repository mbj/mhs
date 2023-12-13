module StackDeploy.Component
  ( Component(..)
  , Mappings
  , namedTemplateFromComponents
) where

import Data.Foldable (fold)
import StackDeploy.Prelude

import qualified Data.Aeson                as JSON
import qualified StackDeploy.NamedTemplate as StackDeploy
import qualified Stratosphere              as CFT

type Mappings = Map Text (Map Text JSON.Object)

-- | Template component that can be merged with other components to forma a full template
--
-- This is intentionally close but not quite a full stratosphere template.
-- Primary use case is to merge together many components, usually defined in separate modules
-- to a full template.
data Component = Component
  { conditions :: JSON.Object
  , outputs    :: CFT.Outputs
  , parameters :: CFT.Parameters
  , resources  :: CFT.Resources
  , mappings   :: Map Text (Map Text JSON.Object)
  }
  deriving stock (Eq, Show)

instance Semigroup Component where
  left <> right = Component
    { conditions = left.conditions <> right.conditions
    , outputs    = left.outputs    <> right.outputs
    , parameters = left.parameters <> right.parameters
    , resources  = left.resources  <> right.resources
    , mappings   = left.mappings   <> right.mappings
    }

instance Monoid Component where
  mempty = Component
   { conditions = []
   , mappings   = []
   , outputs    = []
   , parameters = []
   , resources  = []
   }

-- | Construct named template from components
-- >>> import StackDeploy.Prelude
-- >>> import qualified StackDeploy.Component as StackDeploy
-- >>> import qualified Stratosphere          as CFT
-- >>> let componentA = mempty { StackDeploy.outputs = [CFT.mkOutput "OutputA" "Value-A"] }
-- >>> let componentB = mempty { StackDeploy.outputs = [CFT.mkOutput "OutputB" "Value-B"] }
-- >>> StackDeploy.namedTemplateFromComponents (fromType @"test") [componentA, componentB]
-- NamedTemplate {name = BoundText "test", template = Template {conditions = Just (fromList []), description = Nothing, formatVersion = Nothing, mappings = Just (fromList []), metadata = Nothing, outputs = Just (Outputs {outputs = [Output {name = "OutputA", condition = Nothing, description = Nothing, value = Literal "Value-A", export = Nothing},Output {name = "OutputB", condition = Nothing, description = Nothing, value = Literal "Value-B", export = Nothing}]}), parameters = Just (Parameters {parameterList = []}), resources = Resources {resourceList = []}}}
namedTemplateFromComponents
 :: StackDeploy.TemplateName
 -> [Component]
 -> StackDeploy.NamedTemplate
namedTemplateFromComponents name components
  = StackDeploy.mkNamedTemplate name
  $ (CFT.mkTemplate merged.resources)
  & CFT.set @"Conditions" merged.conditions
  & CFT.set @"Mappings"   merged.mappings
  & CFT.set @"Outputs"    merged.outputs
  & CFT.set @"Parameters" merged.parameters
  where
    merged = fold components
