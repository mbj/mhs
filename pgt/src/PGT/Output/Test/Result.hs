module PGT.Output.Test.Result
  ( Result(..)
  , RowResults(..)
  , parse
  , recordsCount
  , testTree
  )
where

import Data.Attoparsec.Text (Parser)
import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.Word (Word16)
import PGT.Output.Render
import PGT.Output.RowCount (RowCount(..))
import PGT.Output.Text
import PGT.Prelude

import qualified Data.Attoparsec.Text as Text
import qualified Data.Foldable        as Foldable
import qualified Data.List.NonEmpty   as NonEmpty
import qualified Data.Text            as Text
import qualified PGT.Output.Golden    as PGT
import qualified PGT.Output.RowCount  as RowCount
import qualified System.Path          as Path
import qualified Test.Tasty           as Tasty

data Result
  = Empty
  | Error   ErrorResult
  | Records (NonEmpty Record)
  | Rows    RowResults
  deriving stock (Eq, Show)

instance Render Result where
  render = \case
    Empty           -> render $ RowCount 0
    Error   error   -> render error
    Records records -> unlines $ render <$> records
    Rows    table   -> render table

data ErrorResult = ErrorResult
  { error   :: Text
  , details :: [Text]
  }
  deriving stock (Eq, Show)

instance Render ErrorResult where
  render ErrorResult{..} = unlines $ error : details

data Record = Record
  { title :: Text
  , items :: NonEmpty Text
  }
  deriving stock (Eq, Show)

instance Render Record where
  render Record{..}
    = unlines $ convert title <| items

data RowResults = RowResults
  { columns  :: Text
  , rowCount :: RowCount
  , rows     :: [Text]
  }
  deriving stock (Eq, Show)

instance Render RowResults where
  render RowResults{..}
    = unlines $ convert columns : results
    where
      results :: [Text]
      results = case rows of
        [] -> [render rowCount]
        _  -> [unlines rows, render rowCount]

parse :: Parser Result
parse =
  Text.choice
    [ parseEmpty
    , parseError
    , parseRecords
    , parseRows
    ]

parseEmpty :: Parser Result
parseEmpty = Empty <$ "(0 rows)" <* Text.endOfLine

parseError :: Parser Result
parseError = Error <$> parseErrorResult
  where
    parseErrorResult :: Parser ErrorResult
    parseErrorResult = do
      error   <- parseMessage "ERROR:" <|> parseMessage "WARNING:"
      details <- Text.many' parseErrorDetails

      pure ErrorResult{..}
      where
        parseErrorDetails :: Parser Text
        parseErrorDetails
          = Text.choice
          [ parseMessage "ERROR:"
          , parseMessage "DETAIL:"
          , parseMessage "CONTEXT:"
          , parseLineError
          ]

        -- This parser is quite specialized and may need to be modified in future
        parseLineError :: Parser Text
        parseLineError = do
          firstLine <- parseMessage "LINE"
          padding   <- parsePadding
          followup  <- parseLineChars
          pure $ unlines @[] [firstLine, padding <> followup]

        parseMessage :: Text -> Parser Text
        parseMessage prefix = (prefix <>) <$> (Text.string prefix *> parseLineChars)

parseRecords :: Parser Result
parseRecords = Records . NonEmpty.fromList <$> Text.many1' parseRecord
  where
    parseRecord :: Parser Record
    parseRecord = do
      title <- parseTitle

      Record title . NonEmpty.fromList <$> Text.many1' parseItem
      where
        parseTitle  :: Parser Text
        parseTitle  = do
          prefix         <- "-[ RECORD "
          RowCount count <- convertImpure @RowCount <$> Text.scientific
          remaining      <- parseLineChars

          pure $ prefix <> showc count <> remaining

        parseItem :: Parser Text
        parseItem = parseJson <|> parseRecordTextLine
          where
            parseJson :: Parser Text
            parseJson = mkJsonParser '[' <|> mkJsonParser '{'
              where
                mkJsonParser :: Char -> Parser Text
                mkJsonParser openingChar = do
                  paddedColumn <- parsePaddedColumnName
                  openingChars <- beginJson
                  content      <- Text.many' parseJsonChars

                  pure $ unlines ((paddedColumn <> openingChars) : content)
                  where
                    beginJson :: Parser Text
                    beginJson = do
                      beginChar <- Text.space *> Text.char openingChar
                      padding   <- parsePadding <* Text.char '+' <* Text.endOfLine

                      pure $ "| " <> Text.singleton beginChar <> padding <> "+"

                    parseJsonChars :: Parser Text
                    parseJsonChars = do
                      prePadding <- parsePadding <* Text.char '|'
                      content    <- parseLineChars

                      pure $ prePadding <> "|" <> content

            parseRecordTextLine :: Parser Text
            parseRecordTextLine = do
              paddedColumn <- parsePaddedColumnName

              parseEmptyValue paddedColumn <|> parseValue paddedColumn
              where
                parseEmptyValue :: Text -> Parser Text
                parseEmptyValue column = Text.endOfLine $> (column <> "|")

                parseValue :: Text -> Parser Text
                parseValue column = ((column <> "|") <>) <$> parseLineChars

            parsePaddedColumnName :: Parser Text
            parsePaddedColumnName = do
              name    <- parseName
              padding <- parsePadding <* Text.char '|'

              pure $ name <> padding

parseRows :: Parser Result
parseRows = do
  columns         <- parseColumns
  columnUnderLine <- parseUnderLine
  rows            <- parseRowLines
  rowCount        <- RowCount.parse "("

  let title = unlines @[] [columns, columnUnderLine]

  pure . Rows $ RowResults{ columns = title, .. }
  where
    parseUnderLine :: Parser Text
    parseUnderLine = Text.takeWhile1 isUnderLineChar <* Text.endOfLine
      where
        isUnderLineChar char = char == '-' || char == '+'

    parseColumns :: Parser Text
    parseColumns = do
      padding <- parsePadding
      columns <- parseLineChars
      pure $ padding <> columns

    parseRowLines :: Parser [Text]
    parseRowLines = Text.many' parseRowLine
      where
        parseRowLine :: Parser Text
        parseRowLine = do
          space <- Text.space
          (Text.singleton space <>) <$> parseLineChars

recordsCount :: NonEmpty Record -> RowCount
recordsCount = RowCount . convertImpure @Word16 . Foldable.length

testTree :: IO Tasty.TestTree
testTree
  = PGT.mkDirGolden (Path.relDir "src/PGT/Output/Test/Result/examples")
  $ Text.parseOnly parse
