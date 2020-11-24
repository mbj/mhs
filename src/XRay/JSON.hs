module XRay.JSON where

import XRay.Prelude
import qualified Data.Aeson as JSON

jsonOptions :: JSON.Options
jsonOptions = JSON.defaultOptions
  { JSON.constructorTagModifier = JSON.camelTo2 '_'
  , JSON.fieldLabelModifier     = renameType . JSON.camelTo2 '_'
  , JSON.omitNothingFields      = True
  }
   where
     renameType :: String -> String
     renameType = \case
       "type'" -> "type"
       other   -> other
