module PGT.Output.Test.Comments
  ( Comment
  , Commentary(..)
  , Comments
  , MetaComment(..)
  , parseComment
  , parseComments
  , testTree
  )
where

import Control.Monad (unless)
import Data.Attoparsec.Text (Parser)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Set.NonEmpty (NESet)
import PGT.Output.Render
import PGT.Output.RowCount (RowCount(..))
import PGT.Output.Text
import PGT.Prelude

import qualified Data.Attoparsec.Text as Text
import qualified Data.Char            as Char
import qualified Data.Foldable        as Foldable
import qualified Data.List.NonEmpty   as NonEmpty
import qualified Data.Set.NonEmpty    as Set.NonEmpty
import qualified Data.Text            as Text
import qualified PGT.Output.Golden    as PGT
import qualified PGT.Output.RowCount  as RowCount
import qualified System.Path          as Path
import qualified Test.Tasty           as Tasty

data MetaComment
  = ErrorMetaComment
  | FailureMetaComment  RowCount
  | RowCountMetaComment RowCount
  | SuccessMetaComment  RowCount
  deriving stock (Eq, Show)

instance Render MetaComment where
  render = \case
    SuccessMetaComment rowsCount  -> renderWithRowCount rowsCount "success"
    ErrorMetaComment              -> "(ERROR)"
    FailureMetaComment  rowsCount -> renderWithRowCount rowsCount "failure"
    RowCountMetaComment rowCount  -> render rowCount
    where
      renderWithRowCount :: RowCount -> Text -> Text
      renderWithRowCount (RowCount count) text =
        "(" <>  text <> ", " <> showc count <> suffix
        where
          suffix :: Text
          suffix
            | count == 1 = " row)"
            | otherwise  = " rows)"

data Commentary a = Commentary
  { metaComment :: MetaComment
  , text        :: a
  }
  deriving stock (Eq, Show)

type Comments = Commentary (NonEmpty Text)
type Comment  = Commentary Text

instance Render Comments where
  render Commentary{..} = unlines $ commentsList <> [metaCommentText]
    where
      commentsList :: [Text]
      commentsList = Foldable.toList $ mkComment <$> text

      metaCommentText :: Text
      metaCommentText = mkComment $ render metaComment

      mkComment :: Text -> Text
      mkComment = ("-- " <>)

instance Render Comment where
  render Commentary{..} = "-- " <> text <> render metaComment

parseComment :: Parser Comment
parseComment = do
  text <- "-- " *> Text.manyTill' (Text.satisfy Char.isPrint) parseMetaCommentAnchor

  (`Commentary` convert text) <$> parseMetaComment mempty
  where
    parseMetaCommentAnchor :: Parser ()
    parseMetaCommentAnchor = do
      void $ Text.char '('

      char <- Text.peekChar'

      unless (Char.isDigit char || Foldable.any (char ==) acceptedFirstChars)
        $ (errorP . ("expected row count or error text such as (1 row) or (ERROR) but found: (" <>))
        =<< parseLineChars
      where
        acceptedFirstChars :: NESet Char.Char
        acceptedFirstChars = Set.NonEmpty.fromList $ 'E' :| ['f', 's']

parseComments :: Parser Comments
parseComments = do
  text <- NonEmpty.fromList <$> Text.many1' parseCommentLine

  metaComment <-
    eitherImpureError
      =<< Text.eitherP parseUnexpectedEmptyLine (parseMetaComment "-- (")

  pure Commentary{..}
  where
    parseCommentLine :: Parser Text
    parseCommentLine = do
      peekedChar <- Text.peekChar'

      case peekedChar of
        '-'                         -> parseLine <|> parseUnexpectedFirstChar
        char | Char.isAlphaNum char -> errorP . (message <>) =<< parseLineChars
        _                           -> failP . (message <>) =<< parseLineChars
      where
        message :: Text
        message = "expected valid test comment such as \"-- my test comment\" but received: \n\n"

        parseLine :: Parser Text
        parseLine
          = eitherImpureError
          =<< Text.eitherP parseUnexpectedEmptyComment parseExpected
          where
            parseExpected :: Parser Text
            parseExpected =  do
              head <- "-- " *> Text.satisfy Char.isAlphaNum
              (Text.singleton head <>) <$> parseLineChars

        parseUnexpectedFirstChar :: Parser Text
        parseUnexpectedFirstChar = do
          void $ "-- " *> Text.satisfy (not . expectedFirstChar)

          failP . ("expected a comment that starts with an alpha-num char but received:\n\n" <>)
            =<< parseLineChars
          where
            expectedFirstChar :: Char -> Bool
            expectedFirstChar char = Char.isAlphaNum char || char == '('

        parseUnexpectedEmptyComment :: Parser Text
        parseUnexpectedEmptyComment =
          "--"
            *> Text.endOfLine
            $> "expected comment text after \"--\" but found none"

    parseUnexpectedEmptyLine :: Parser Text
    parseUnexpectedEmptyLine =
      (Text.endOfLine <|> Text.endOfInput)
        $> "expected a row-count comment such as (0 rows) or (ERROR) comment but received empty line"

parseMetaComment :: Text -> Parser MetaComment
parseMetaComment prefix
  = Text.choice
    [ parseErrorComment
    , parseFailureComment
    , parseRowCount
    , parseSuccessComment
    , parseUnexpected
    ]
  where
    parseErrorComment :: Parser MetaComment
    parseErrorComment = Text.string prefix *> "ERROR)" *> Text.endOfLine $> ErrorMetaComment

    parseFailureComment :: Parser MetaComment
    parseFailureComment = FailureMetaComment <$> RowCount.parse (prefix <> "failure, ")

    parseRowCount :: Parser MetaComment
    parseRowCount = RowCountMetaComment <$> RowCount.parse prefix

    parseSuccessComment :: Parser MetaComment
    parseSuccessComment = SuccessMetaComment <$> RowCount.parse (prefix <> "success, ")

    parseUnexpected :: Parser MetaComment
    parseUnexpected =  do
      text <- Text.string prefix *> parseLineChars
      error $ prefix <> text
      where
        error :: Text -> Parser a
        error
          = errorP
          . ("expected valid meta comment such as (ERROR) or (1 row) but received: \n\n" <>)

testTree :: IO Tasty.TestTree
testTree = do
  commentsTree <-
    PGT.mkDirGolden (Path.relDir "src/PGT/Output/Test/Comments/examples/comments")
      $ Text.parseOnly parseComments

  commentTree <-
    PGT.mkDirGolden (Path.relDir "src/PGT/Output/Test/Comments/examples/comment")
      $ Text.parseOnly parseComment

  pure $ Tasty.testGroup "Comments"
    [ commentTree
    , commentsTree
    ]
