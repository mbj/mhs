module PGT.Output.RowCount
  ( RowCount(..)
  )
where

import Control.Monad.Error.Class (MonadError(..))
import Data.Scientific (Scientific)
import Data.Tuple (fst)
import Data.Word (Word8)
import GHC.Real (properFraction)
import PGT.Output.Render
import PGT.Prelude

import qualified Data.Scientific as Scientific

newtype RowCount = RowCount Word8
  deriving stock (Eq, Show)

newtype ParseError = ParseError Text
  deriving stock (Eq, Show)

instance MonadError ParseError m => Conversion (m RowCount) Scientific where
  convert = either throwError (pure . RowCount) . parseWord8
    where
      parseWord8 :: Scientific -> Either ParseError Word8
      parseWord8 scientific
        | scientific < 0                   =
            Left . ParseError $ "expected non negative integral but received: " <> showc scientific
        | natural > convert maxWord8       =
            Left . ParseError
              $ "expected row count to be a Word8 and hence less than: "
              <> showc maxWord8
              <> " but received "
              <> showc scientific
        | Scientific.isFloating scientific =
            Left . ParseError $ "expected integral but received float: " <> showc scientific
        | otherwise                        =
            pure $ convertImpure natural
        where
          maxWord8 = maxBound @Word8

          natural :: Natural
          natural = fst $ properFraction scientific

instance Render RowCount where
  render (RowCount count)
    | count == 1 = "(1 row)"
    | otherwise  = "(" <> showc count <> " rows)"
