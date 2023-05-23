module PGT.Output.Test.Comments
  ( Comments(..)
  , MetaComment(..)
  , parse
  , testTree
  )
where

import Data.Attoparsec.Text (Parser)
import Data.List.NonEmpty (NonEmpty(..))
import PGT.Output.Render
import PGT.Output.RowCount (RowCount(..))
import PGT.Output.Text
import PGT.Prelude

import qualified Data.Attoparsec.Text as Text
import qualified Data.Char            as Char
import qualified Data.Foldable        as Foldable
import qualified Data.List.NonEmpty   as NonEmpty
import qualified Data.Text            as Text
import qualified GHC.Err              as Err
import qualified PGT.Output.Golden    as PGT
import qualified PGT.Output.RowCount  as RowCount
import qualified System.Path          as Path
import qualified Test.Tasty           as Tasty

data MetaComment
  = ErrorMetaComment
  | RowCountMetaComment RowCount
  deriving stock (Eq, Show)

instance Render MetaComment where
  render = \case
    ErrorMetaComment             -> "(ERROR)"
    RowCountMetaComment rowCount -> render rowCount

data Comments = Comments
  { comments    :: NonEmpty Text
  , metaComment :: MetaComment
  }
  deriving stock (Eq, Show)

instance Render Comments where
  render Comments{..} = unlines $ commentsList <> [metaCommentText]
    where
      commentsList :: [Text]
      commentsList = Foldable.toList $ mkComment <$> comments

      metaCommentText :: Text
      metaCommentText = mkComment $ render metaComment

      mkComment :: Text -> Text
      mkComment = ("-- " <>)

parse :: Parser Comments
parse = do
  comments <- NonEmpty.fromList <$> Text.many1' parseCommentLine

  metaComment <-
    either Err.error pure
      =<< Text.eitherP parseUnexpectedEmptyLine parseMetaComment

  pure Comments{..}
  where
    parseCommentLine :: Parser Text
    parseCommentLine = do
      peekedChar <- Text.peekChar'

      case peekedChar of
        '-'                         -> parseLine <|> parseUnexpectedFirstChar
        char | Char.isAlphaNum char -> Err.error . (message <>) . convert <$> parseLineChars
        _                           -> fail . (message <>) . convert =<< parseLineChars
      where
        message :: String
        message = "expected valid test comment such as \"-- my test comment\" but received: \n\n"

        parseLine :: Parser Text
        parseLine
          = either Err.error pure
          =<< Text.eitherP parseUnexpectedEmptyComment parseExpected
          where
            parseExpected :: Parser Text
            parseExpected =  do
              head <- "-- " *> Text.satisfy Char.isAlphaNum
              (Text.singleton head <>) <$> parseLineChars

        parseUnexpectedFirstChar :: Parser Text
        parseUnexpectedFirstChar = do
          void $ "-- " *> Text.satisfy (not . expectedFirstChar)

          fail . ("expected a comment that starts with an alpha-num char but received:\n\n" <>) . convert
            =<< parseLineChars
          where
            expectedFirstChar :: Char -> Bool
            expectedFirstChar char = Char.isAlphaNum char || char == '('

        parseUnexpectedEmptyComment :: Parser String
        parseUnexpectedEmptyComment =
          "--"
            *> Text.endOfLine
            $> "expected comment text after \"--\" but found none"

    parseUnexpectedEmptyLine :: Parser String
    parseUnexpectedEmptyLine =
      (Text.endOfLine <|> Text.endOfInput)
        $> "expected a row-count comment such as (0 rows) or (ERROR) comment but received empty line"

parseMetaComment :: Parser MetaComment
parseMetaComment = parseErrorComment <|> parseRowCount <|> parseUnexpected
  where
    parseErrorComment :: Parser MetaComment
    parseErrorComment = "-- (ERROR)" *> Text.endOfLine $> ErrorMetaComment

    parseRowCount :: Parser MetaComment
    parseRowCount = "-- " *> (RowCountMetaComment <$> RowCount.parse "(")

    parseUnexpected :: Parser MetaComment
    parseUnexpected =  do
      text <- "-- (" *> parseLineChars
      error $ "-- (" <> text
      where
        error :: Text -> b
        error
          = Err.error
          . ("expected valid meta comment such as (ERROR) or (1 row) but received: \n\n" <>)
          . convert

testTree :: IO Tasty.TestTree
testTree
  = PGT.mkDirGolden (Path.relDir "src/PGT/Output/Test/Comments/examples/comments")
  $ Text.parseOnly parse
