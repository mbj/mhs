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
    { conditions = getField @"conditions" left <> getField @"conditions" right
    , outputs    = getField @"outputs"    left <> getField @"outputs"    right
    , parameters = getField @"parameters" left <> getField @"parameters" right
    , resources  = getField @"resources"  left <> getField @"resources"  right
    , mappings   = getField @"mappings"   left <> getField @"mappings"   right
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
