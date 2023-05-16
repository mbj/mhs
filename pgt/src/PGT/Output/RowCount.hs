module PGT.Output.RowCount
  ( RowCount(..)
  , parse
  )
where

import Control.Monad.Error.Class (MonadError(..))
import Data.Attoparsec.Text (Parser)
import Data.Scientific (Scientific)
import Data.Tuple (fst)
import Data.Word (Word16)
import GHC.Real (properFraction)
import PGT.Output.Render
import PGT.Prelude

import qualified Data.Attoparsec.Text as Text
import qualified Data.Scientific      as Scientific
import qualified GHC.Err              as Err

newtype RowCount = RowCount Word16
  deriving stock (Eq, Show)

newtype ParseError = ParseError Text
  deriving stock (Eq, Show)

instance MonadError ParseError m => Conversion (m RowCount) Scientific where
  convert = either throwError (pure . RowCount) . parseWord8
    where
      parseWord8 :: Scientific -> Either ParseError Word16
      parseWord8 scientific
        | scientific < 0                   =
            Left . ParseError $ "expected non negative integral but received: " <> showc scientific
        | natural > convert maxWord16       =
            Left . ParseError
              $ "expected row count to be a Word8 and hence less than: "
              <> showc maxWord16
              <> " but received "
              <> showc scientific
        | Scientific.isFloating scientific =
            Left . ParseError $ "expected integral but received float: " <> showc scientific
        | otherwise                        =
            pure $ convertImpure natural
        where
          maxWord16 = maxBound @Word16

          natural :: Natural
          natural = fst $ properFraction scientific

instance Render RowCount where
  render (RowCount count)
    | count == 1 = "(1 row)"
    | otherwise  = "(" <> showc count <> " rows)"

parse :: Text -> Parser RowCount
parse prefix = do
  rowCount <- Text.string prefix *> (convertImpure <$> Text.scientific)

  validateRowString rowCount =<< Text.space *> ("rows" <|> "row")

  Text.char ')' *> Text.endOfLine $> rowCount
  where
    validateRowString :: RowCount -> Text -> Parser ()
    validateRowString rowCount@(RowCount count) string
      | count == 1 && string /= "row"  = Err.error "expected (1 row) but found (1 rows)"
      | count /= 1 && string /= "rows" = Err.error message
      | otherwise                      = pure ()
      where
        message :: String
        message
          = convert
          $ "expected " <> render rowCount <> " but found (" <> showc count <> " row)"
