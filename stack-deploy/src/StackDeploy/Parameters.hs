{-# OPTIONS -Wwarn #-}

module StackDeploy.Parameters
  ( Parameter(..)
  , ParameterExpand(..)
  , ParameterMap
  , ParameterName
  , ParameterValue
  , parameterFromStratosphere
  , parameterMapFromList
  , parameterMapTemplateExpand
  )
where

import Control.Lens ((?~))
import StackDeploy.Prelude

import qualified Amazonka.CloudFormation.Types as CF
import qualified Data.Foldable                 as Foldable
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import qualified Data.Text                     as Text
import qualified Stratosphere                  as CFT

-- $setup
-- >>> :{
-- import Control.Lens ((?~))
-- import StackDeploy.Prelude
-- import qualified Amazonka.CloudFormation.Types as CF
-- import qualified StackDeploy.Parameters        as StackDeploy
-- import qualified Stratosphere                  as CFT
-- :}

type ParameterName  = BoundText "StackDeploy.Parameter.Name"
type ParameterValue = BoundText "StackDeploy.Parameter.Value"

data Parameter = Parameter
  { name  :: ParameterName
  , value :: ParameterValue
  }
  deriving stock (Eq, Show)

type ParameterMap = Map ParameterName ParameterValue

-- | Create parameter from stratosphere
-- >>> StackDeploy.parameterFromStratosphere (CFT.mkParameter "A" "String") (fromType @"A-Value")
-- Parameter {name = BoundText "A", value = BoundText "A-Value"}
parameterFromStratosphere
  :: CFT.Parameter
  -> ParameterValue
  -> Parameter
parameterFromStratosphere = Parameter . convertImpure . (.name)

parameterMapFromList :: [Parameter] -> ParameterMap
parameterMapFromList = Map.fromList . fmap (\Parameter{..} -> (name, value))

data ParameterExpand
  = ParameterExpandCreate
  | ParameterExpandUpdate [CF.Parameter]

-- | Expand parameter map to amazonka cloudformation parameters against a stratosphere template
-- |
-- | Explansion fails if unknown parameters are provided.
-- | Missing parameters will not be detected. These are to be detected by the cloudformation API.
-- >>> let cfParameterA  = CF.newParameter & CF.parameter_parameterKey ?~ "A"
-- >>> let cfParameterB  = CF.newParameter & CF.parameter_parameterKey ?~ "B"
-- >>> let cftParameterA = CFT.mkParameter "A" "String" & CFT.set @"Default" "A-Default"
-- >>> let cftParameterB = CFT.mkParameter "B" "String"
-- >>> let cftTemplate   = CFT.mkTemplate [] & CFT.set @"Parameters" [cftParameterA, cftParameterB]
-- >>> let pairA         = (fromType @"A", fromType @"A-Value")
-- >>> let pairB         = (fromType @"B", fromType @"B-Value")
-- >>> let pairC         = (fromType @"C", fromType @"C-Value")
-- >>> StackDeploy.parameterMapTemplateExpand StackDeploy.ParameterExpandCreate [pairC] cftTemplate
-- Left "Unknown parameters: C"
-- >>> StackDeploy.parameterMapTemplateExpand StackDeploy.ParameterExpandCreate [pairA] cftTemplate
-- Right [Parameter' {parameterKey = Just "A", parameterValue = Just "A-Value", resolvedValue = Nothing, usePreviousValue = Nothing}]
-- >>> StackDeploy.parameterMapTemplateExpand (StackDeploy.ParameterExpandUpdate [cfParameterA, cfParameterB]) [pairA] cftTemplate
-- Right [Parameter' {parameterKey = Just "A", parameterValue = Just "A-Value", resolvedValue = Nothing, usePreviousValue = Nothing},Parameter' {parameterKey = Just "B", parameterValue = Nothing, resolvedValue = Nothing, usePreviousValue = Just True}]
-- >>> StackDeploy.parameterMapTemplateExpand (StackDeploy.ParameterExpandUpdate [cfParameterA, cfParameterB]) [pairB] cftTemplate
-- Right [Parameter' {parameterKey = Just "A", parameterValue = Nothing, resolvedValue = Nothing, usePreviousValue = Just True},Parameter' {parameterKey = Just "B", parameterValue = Just "B-Value", resolvedValue = Nothing, usePreviousValue = Nothing}]
-- >>> StackDeploy.parameterMapTemplateExpand (StackDeploy.ParameterExpandUpdate [cfParameterB]) [pairB] cftTemplate
-- Right [Parameter' {parameterKey = Just "B", parameterValue = Just "B-Value", resolvedValue = Nothing, usePreviousValue = Nothing}]
parameterMapTemplateExpand
  :: ParameterExpand
  -> ParameterMap
  -> CFT.Template
  -> Either String [CF.Parameter]
parameterMapTemplateExpand operation parameterMap template
  = if not $ Set.null unknownParameterNames
      then Left $ "Unknown parameters: " <> join unknownParameterNames
      else pure $ Foldable.foldMap process $ Set.toList templateParameterNames
  where
    process :: ParameterName -> [CF.Parameter]
    process parameterName =
      maybe absent (pure . mkParameterValue) (Map.lookup parameterName parameterMap)
      where
        absent = case operation of
          ParameterExpandCreate   -> []
          ParameterExpandUpdate{} -> [mkUsePrevious | Set.member parameterName previousParameterNames]

        parameterNameText = convert parameterName

        mkParameterValue :: ParameterValue -> CF.Parameter
        mkParameterValue parameterValue
          = CF.newParameter
          & CF.parameter_parameterKey   ?~ parameterNameText
          & CF.parameter_parameterValue ?~ convert parameterValue

        mkUsePrevious :: CF.Parameter
        mkUsePrevious
          = CF.newParameter
          & CF.parameter_parameterKey     ?~ parameterNameText
          & CF.parameter_usePreviousValue ?~ True

    unknownParameterNames :: Set ParameterName
    unknownParameterNames = Set.difference givenParameterNames templateParameterNames

    givenParameterNames :: Set ParameterName
    givenParameterNames = Map.keysSet parameterMap

    templateParameterNames
      = Set.fromList
      $ convertImpure . (.name) <$> maybe [] (.parameterList) template.parameters

    previousParameterNames = case operation of
      ParameterExpandCreate ->
        []
      (ParameterExpandUpdate cfPreviousParameters) ->
        Set.fromList $ Foldable.foldMap (maybe [] (pure . convertImpure) . (.parameterKey)) cfPreviousParameters

    join :: Set ParameterName -> String
    join = convert . Text.intercalate "," . fmap convert . Set.toList
