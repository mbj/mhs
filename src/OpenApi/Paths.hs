module OpenApi.Paths where

import GHC.Generics (Generic)
import OpenApi.JSON
import OpenApi.Prelude

import qualified Data.Aeson      as JSON
import qualified Data.Map.Strict as Map
import qualified GHC.Enum        as GHC

data Operation = Operation
  { description :: OperationDescription
  , operationId :: OperationId
  , parameters  :: Maybe [Parameter]
  }
  deriving anyclass JSON.FromJSON
  deriving stock    (Generic, Show)

newtype OperationDescription = OperationDescription Text
  deriving newtype JSON.FromJSON
  deriving stock   Show

newtype OperationId = OperationId Text
  deriving newtype JSON.FromJSON
  deriving stock   Show

data Parameter = Parameter
  { description :: Maybe ParameterDescription
  , location    :: ParameterLocation
  , name        :: ParameterName
  , required    :: Bool
  , style       :: ParameterStyle
  }
  deriving stock (Generic, Show)

instance JSON.FromJSON Parameter where
  parseJSON = parseRenamed $ Map.singleton "location" "in"

newtype ParameterDescription = ParameterDescription Text
  deriving newtype JSON.FromJSON
  deriving stock   Show

data ParameterLocation = Cookie | Header | Path | Query
  deriving stock (GHC.Bounded, GHC.Enum, Show)

instance JSON.FromJSON ParameterLocation where
  parseJSON = parseJSONFixed "ParameterLocation" JSON.withText $ \case
    Cookie -> "cookie"
    Header -> "header"
    Path   -> "path"
    Query  -> "query"

newtype ParameterName = ParameterName Text
  deriving newtype JSON.FromJSON
  deriving stock   Show

data ParameterStyle = DeepObject | Form | Simple
  deriving stock (GHC.Bounded, GHC.Enum, Show)

instance JSON.FromJSON ParameterStyle where
  parseJSON = parseJSONFixed "ParameterStyle" JSON.withText $ \case
    DeepObject -> "deepObject"
    Form       -> "form"
    Simple     -> "simple"

newtype PathName = PathName Text
  deriving newtype (Eq, JSON.FromJSONKey, Ord)
  deriving stock   Show

data Item = Item
  { delete  :: Maybe Operation
  , get     :: Maybe Operation
  , head    :: Maybe Operation
  , options :: Maybe Operation
  , post    :: Maybe Operation
  , put     :: Maybe Operation
  , patch   :: Maybe Operation
  }
  deriving anyclass JSON.FromJSON
  deriving stock    (Generic, Show)
