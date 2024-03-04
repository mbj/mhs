{-# OPTIONS -Wwarn #-}

module StackDeploy.Parameters
  ( Parameter(..)
  , ParameterExpand(..)
  , ParameterMap
  , ParameterName
  , ParameterValue
  , parameterFromStratosphere
  , parameterMapExpand
  , parameterMapFromList
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

type ParameterName  = BoundText' "StackDeploy.Parameter.Name" '(1, 255)
type ParameterValue = BoundText' "StackDeploy.Parameter.Value" '(0, 4096)

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
  | ParameterExpandUpdate (Set ParameterName)

-- | Expand parameter map to amazonka cloudformation parameters against a stratosphere template
-- |
-- | Explansion fails if unknown parameters are provided.
-- | Missing parameters will not be detected. These are to be detected by the cloudformation API.
-- >>> parameterNameA         = fromType @"A"
-- >>> parameterNameB         = fromType @"B"
-- >>> templateParameterNames = [parameterNameA, parameterNameB]
-- >>> let pairA              = (parameterNameA, fromType @"A-Value")
-- >>> let pairB              = (parameterNameB, fromType @"B-Value")
-- >>> let pairC              = (fromType @"C", fromType @"C-Value")
-- >>> StackDeploy.parameterMapExpand StackDeploy.ParameterExpandCreate templateParameterNames [pairC]
-- Left "Unknown parameters: C"
-- >>> StackDeploy.parameterMapExpand StackDeploy.ParameterExpandCreate templateParameterNames [pairA]
-- Right [Parameter' {parameterKey = Just "A", parameterValue = Just "A-Value", resolvedValue = Nothing, usePreviousValue = Nothing}]
-- >>> StackDeploy.parameterMapExpand (StackDeploy.ParameterExpandUpdate [parameterNameA, parameterNameB]) templateParameterNames [pairA]
-- Right [Parameter' {parameterKey = Just "A", parameterValue = Just "A-Value", resolvedValue = Nothing, usePreviousValue = Nothing},Parameter' {parameterKey = Just "B", parameterValue = Nothing, resolvedValue = Nothing, usePreviousValue = Just True}]
-- >>> StackDeploy.parameterMapExpand (StackDeploy.ParameterExpandUpdate [parameterNameA, parameterNameB]) templateParameterNames [pairB]
-- Right [Parameter' {parameterKey = Just "A", parameterValue = Nothing, resolvedValue = Nothing, usePreviousValue = Just True},Parameter' {parameterKey = Just "B", parameterValue = Just "B-Value", resolvedValue = Nothing, usePreviousValue = Nothing}]
-- >>> StackDeploy.parameterMapExpand (StackDeploy.ParameterExpandUpdate [parameterNameB]) templateParameterNames [pairB]
-- Right [Parameter' {parameterKey = Just "B", parameterValue = Just "B-Value", resolvedValue = Nothing, usePreviousValue = Nothing}]
parameterMapExpand
  :: ParameterExpand
  -> Set ParameterName
  -> ParameterMap
  -> Either String [CF.Parameter]
parameterMapExpand operation templateParameterNames parameterMap
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

    previousParameterNames = case operation of
      ParameterExpandCreate                           -> []
      (ParameterExpandUpdate previousParameterNames') -> previousParameterNames'

    join :: Set ParameterName -> String
    join = convert . Text.intercalate "," . fmap convert . Set.toList
