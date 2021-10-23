module OpenApi.Validation (Flag(..), validate) where

import Control.Monad (unless)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Control.Monad.Writer.Strict (WriterT, execWriterT, tell)
import Data.Maybe (isJust)
import Data.Set (Set)
import OpenApi.Components
import OpenApi.MediaType
import OpenApi.OpenApi
import OpenApi.Operation
import OpenApi.Parameter
import OpenApi.PathItem
import OpenApi.Prelude hiding (not)
import OpenApi.Referencable
import OpenApi.Reference
import OpenApi.ReferenceOr
import OpenApi.RequestBody
import OpenApi.Resolver
import OpenApi.Response
import OpenApi.Responses
import OpenApi.Schema

import qualified Data.Foldable   as Foldable
import qualified Data.List       as List
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

data Flag = Flag
  { path    :: [Text]
  , message :: Text
  }
  deriving stock (Eq, Ord, Show)

data Context = Context
  { spec :: OpenApi
  , path :: [Text]
  }
  deriving stock Show

type Validator a = ReaderT Context (WriterT (Set Flag) Identity) a

validate :: OpenApi -> [Flag]
validate openapi
  = Set.toList
  . runIdentity
  . execWriterT
  $ runReaderT (validateOpenApi openapi) (Context openapi [])

validateOpenApi :: OpenApi -> Validator ()
validateOpenApi OpenApi{..} = do
  visit      "path"       (visitMap visitPathItem) paths
  visitMaybe "components" visitComponents components

visit :: Text -> (a -> Validator ()) -> a -> Validator ()
visit name validator = push name . validator

visitMaybe :: Text -> (a -> Validator ()) -> Maybe a -> Validator ()
visitMaybe name validator = maybe (pure ()) (visit name validator)

visitPresent :: Text -> Maybe a -> (a -> Validator ()) -> Validator ()
visitPresent name value validator =
  maybe (addFlag $ "missing: " <> name) (visit name validator) value

visitComponents :: Components -> Validator ()
visitComponents Components{..} = do
  visitMaybeReferenceMap visitSchema    schemas
  visitMaybeReferenceMap visitResponse  responses
  visitMaybeReferenceMap visitParameter parameters

visitMap
  :: forall a b . ToText a
  => (b -> Validator ())
  -> Map a b
  -> Validator ()
visitMap validator = visitList visitPair . Map.toList
  where
    visitPair :: (a, b) -> Validator ()
    visitPair (key, component) = visit (toText key) validator component

visitMaybeMap
  :: forall a b . ToText a
  => Text
  -> (b -> Validator ())
  -> Maybe (Map a b)
  -> Validator ()
visitMaybeMap name validator = visitMaybe name (visitMap validator)

visitMaybeReferenceMap
  :: forall a b . (ToText a, Resolver b)
  => (b -> Validator ())
  -> Maybe (Map a (ReferenceOr b))
  -> Validator ()
visitMaybeReferenceMap validator
  = visitMaybeMap
    (convertText $ targetName @b)
    (visitReferenceOr validator)

visitPathItem :: PathItem -> Validator ()
visitPathItem PathItem{..} = do
  visitMaybe "get"     visitOperation get
  visitMaybe "head"    visitOperation head
  visitMaybe "post"    visitOperation post
  visitMaybe "options" visitOperation options
  visitMaybe "delete"  visitOperation delete
  visitMaybe "patch"   visitOperation patch
  visitMaybe "put"     visitOperation put

visitOperation :: Operation -> Validator ()
visitOperation Operation{..} = do
  visit      "responses"   visitResponses responses
  visitMaybe "parameters"  (visitList (visitReferenceOr visitParameter)) parameters
  visitMaybe "requestBody" (visitReferenceOr visitRequestBody) requestBody

visitRequestBody :: RequestBody -> Validator ()
visitRequestBody RequestBody{..} =
  visit "content" (visitMap visitMediaType) content

visitResponses :: OpenApi.Responses.Responses -> Validator ()
visitResponses Responses{..} = do
  visitMaybe "default" (visitReferenceOr visitResponse) default'
  visitMap (visitReferenceOr visitResponse) patterns

visitParameter :: Parameter -> Validator ()
visitParameter Parameter{..} =
  visitMaybe "schema" (visitReferenceOr visitSchema) schema

visitResponse :: Response -> Validator ()
visitResponse Response{..} =
  visitMaybeMap "content" visitMediaType content

visitMediaType :: MediaType -> Validator ()
visitMediaType MediaType{..} =
  visitMaybe "schema" (visitReferenceOr visitSchema) schema

visitSchema :: Schema -> Validator ()
visitSchema schema@Schema{..} = do
  visitMaybe "type" (visitType schema) type'
  visitMaybe "allOf" (visitList $ visitReferenceOr visitSchema) allOf
  visitMaybe "anyOf" (visitList $ visitReferenceOr visitSchema) anyOf
  visitMaybe "oneOf" (visitList $ visitReferenceOr visitSchema) oneOf
  visitMaybe "discriminator" (visitDiscriminator schema) discriminator

visitDiscriminator :: Schema -> Discriminator -> Validator ()
visitDiscriminator Schema{..} Discriminator{..} = do
  unless (isJust anyOf || isJust oneOf || isJust allOf)
    $ addFlag @Text "Discriminator used on schema without allOf anyOf or oneOf"
  traverse_ (traverse_ $ visitDiscriminatorMappingKey propertyName) (Map.toList <$> mapping)

visitDiscriminatorMappingKey :: PropertyName -> (DiscriminatorKey, Reference Schema) -> Validator ()
visitDiscriminatorMappingKey propertyName (key, reference)
  = push (toText key)
  . validateReference reference
  $ \Schema{..} -> visitPresent "properties" properties
  $ \map ->
    maybe
      (addFlag $ "missing property: " <> toText propertyName)
      (const $ pure ())
      (Map.lookup propertyName map)

visitType :: Schema -> Type -> Validator ()
visitType schema = \case
  Array  -> visitArray schema
  Object -> visitObject schema
  _      -> pure ()

visitArray :: Schema -> Validator ()
visitArray Schema{..} =
  visitMaybe "items" (visitReferenceOr visitSchema) items

visitObject :: Schema -> Validator ()
visitObject Schema{..} = do
  visitMaybeReferenceMap visitSchema properties
  visitMaybe "required" validateRequired required
  where
    validateRequired :: [PropertyName] -> Validator ()
    validateRequired requiredNames = do
      visitList duplicate extraNames
      visitList mismatched mismatchedNames
      where
        duplicate :: PropertyName -> Validator ()
        duplicate name = addFlag @Text $ "Required property: " <> convertText name <> " is duplicated"

        mismatched :: PropertyName -> Validator ()
        mismatched name = addFlag @Text $ "Required property: " <> convertText name <> " is not specified"

        extraNames  = (List.\\) requiredNames uniqueNames
        uniqueNames = List.nub requiredNames

        propertyNames = maybe [] Map.keys properties

        mismatchedNames = (List.\\) uniqueNames propertyNames

visitReferenceOr
  :: Resolver a
  => (a -> Validator ())
  -> ReferenceOr a
  -> Validator ()
visitReferenceOr validator = \case
  ReferenceTo reference -> validateReference reference validator
  Literal value         -> validator value

validateReference
  :: Resolver a
  => Reference a
  -> (a -> Validator ())
  -> Validator ()
validateReference reference validator = do
  Context{..} <- ask
  either addFlag validator $ resolve spec reference

visitList :: (a -> Validator ()) -> [a] -> Validator ()
visitList = Foldable.traverse_

addFlag :: Show a => a -> Validator ()
addFlag value = do
  Context{..} <- ask
  tell [Flag { message = convertText $ show value , ..  }]

push :: Text -> Validator () -> Validator ()
push name = local (pushPath name)

pushPath :: Text -> Context -> Context
pushPath name Context{..} = Context { path = path <> [name], .. }
