{-# OPTIONS -Wno-redundant-constraints #-}
module Data.Bounded.JSON where

import Data.Bounded.Prelude
import Data.Bounded.TypeLevel
import Data.Scientific (Scientific)

import qualified Data.Aeson       as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.Scientific  as Scientific

parseJSONIntegralBounded
  :: forall b a . (a ~ ToBoundedIntegral b, Conversion b a, Bounded a, Integral a, Integral b, Show b)
  => String
  -> (b, b)
  -> JSON.Value
  -> JSON.Parser b
parseJSONIntegralBounded field (min', max') = JSON.withScientific field parseNumber
  where
    parseNumber :: Scientific -> JSON.Parser b
    parseNumber scientific = do
     number <- maybe (failMessage "Number is out of integral type bounds") pure mNumber
     if | number < min' -> failMessage $ "cannot be less than " <> show min'
        | number > max' -> failMessage $ "cannot be greater than " <> show max'
        | otherwise     -> pure number
      where
        mNumber :: Maybe b
        mNumber = convert @b <$> Scientific.toBoundedInteger @a scientific

        failMessage :: String -> JSON.Parser b
        failMessage message = fail $ "parsing " <> field <> " failed, " <> message
