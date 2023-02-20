module StackDeploy.Component (Component(..), Mappings, mkTemplate) where

import Data.Foldable (fold)
import Data.HashMap.Strict (HashMap)
import StackDeploy.Prelude
import Stratosphere

import qualified Data.Aeson           as JSON
import qualified StackDeploy.Template as Template

type Mappings = HashMap Text (HashMap Text JSON.Object)

data Component = Component
  { conditions :: JSON.Object
  , outputs    :: Outputs
  , parameters :: Parameters
  , resources  :: Resources
  , mappings   :: HashMap Text (HashMap Text JSON.Object)
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
  $ Stratosphere.template resources
  & templateConditions ?~ conditions
  & templateMappings   ?~ mappings
  & templateOutputs    ?~ outputs
  & templateParameters ?~ parameters
  where
    Component{..} = fold components
