module StackDeploy.Component (Component(..), mkTemplate) where

import Data.Foldable (fold)
import StackDeploy.Prelude
import Stratosphere

import qualified Data.Aeson           as JSON
import qualified StackDeploy.Template as Template

data Component = Component
  { conditions :: JSON.Object
  , outputs    :: Outputs
  , parameters :: Parameters
  , resources  :: Resources
  }

instance Semigroup Component where
  left <> right = Component
    { conditions = getField @"conditions" left <> getField @"conditions" right
    , outputs    = getField @"outputs"    left <> getField @"outputs"    right
    , parameters = getField @"parameters" left <> getField @"parameters" right
    , resources  = getField @"resources"  left <> getField @"resources"  right
    }

instance Monoid Component where
  mempty = Component
   { conditions = []
   , outputs    = []
   , parameters = []
   , resources  = []
   }

mkTemplate :: Template.Name -> [Component] -> Template.Template
mkTemplate name components
  = Template.mk name
  $ Stratosphere.template resources
  & templateConditions ?~ conditions
  & templateOutputs    ?~ outputs
  & templateParameters ?~ parameters
  where
    Component{..} = fold components
