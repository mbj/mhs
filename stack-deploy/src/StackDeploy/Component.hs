module StackDeploy.Component (Component(..), Mappings, mkTemplate) where

import Data.Foldable (fold)
import Data.Map.Strict (Map)
import StackDeploy.Prelude

import qualified Data.Aeson           as JSON
import qualified StackDeploy.Template as Template
import qualified Stratosphere

type Mappings = Map Text (Map Text JSON.Object)

data Component = Component
  { conditions :: JSON.Object
  , outputs    :: Stratosphere.Outputs
  , parameters :: Stratosphere.Parameters
  , resources  :: Stratosphere.Resources
  , mappings   :: Map Text (Map Text JSON.Object)
  }

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

mkTemplate :: Template.Name -> [Component] -> Template.Template
mkTemplate name components
  = Template.mk name
  $ (Stratosphere.mkTemplate merged.resources)
  & Stratosphere.set @"Conditions" merged.conditions
  & Stratosphere.set @"Mappings"   merged.mappings
  & Stratosphere.set @"Outputs"    merged.outputs
  & Stratosphere.set @"Parameters" merged.parameters
  where
    merged = fold components
