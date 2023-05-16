module PGT.Output.Text
  ( eitherFail
  , eitherImpureError
  , errorP
  , failP
  , impureParseEmptyLine
  , mkParseEmptyLines
  , parseEmptyLine
  , parseLineChars
  , parseName
  , parsePadding
  , unlines
  , unlinesN
  )
where

import Data.Attoparsec.Text (Parser)
import Data.Int (Int)
import Data.Tuple (fst)
import Data.Word (Word8)
import GHC.Integer (Integer)
import PGT.Prelude

import qualified Data.Attoparsec.Internal.Types as Text
import qualified Data.Attoparsec.Text           as Text
import qualified Data.Char                      as Char
import qualified Data.Foldable                  as Foldable
import qualified Data.Text                      as Text
import qualified GHC.Err                        as Err

eitherFail :: Either Text a -> Parser a
eitherFail = either failP pure

eitherImpureError :: Either Text a -> Parser a
eitherImpureError = either errorP pure

errorP :: Text -> Parser a
errorP = mkParserFailure Err.error

failP :: Text -> Parser a
failP = mkParserFailure fail

mkParserFailure
  :: (String -> Parser a)
  -> Text
  -> Parser a
mkParserFailure onFailure message = do
  linePosition <- getLinePosition

  onFailure
    $ convert message <> " at line: " <> show linePosition
  where
    getLinePosition :: Parser Int
    getLinePosition
      = Text.Parser
      $ \textBuffer position more _failure success ->
            success textBuffer position more . getLineNumber position $ showc textBuffer
      where
        getLineNumber :: Text.Pos -> Text -> Int
        getLineNumber position string
          = Foldable.length
          . Text.splitOn "\\n"
          . fst
          $ Text.splitAt (Text.fromPos position) string

impureParseEmptyLine :: String -> Parser ()
impureParseEmptyLine = either Err.error pure <=< mkParseEmptyLines 1

parseEmptyLine :: String -> Parser ()
parseEmptyLine = either fail pure <=< mkParseEmptyLines 1

mkParseEmptyLines :: Natural -> String -> Parser (Either String ())
mkParseEmptyLines expectedLines message = do
  receivedLines <- Foldable.length <$> Text.many' Text.endOfLine

  if convertImpure receivedLines == expectedLines
    then pure $ pure ()
    else pure
      . Left
      $ "found " <> show receivedLines <> " empty lines " <> message <> " instead of " <> show expectedLines

parseLineChars :: Parser Text
parseLineChars = Text.takeWhile1 Char.isPrint <* Text.endOfLine

parseName :: Parser Text
parseName = do
  head <- Text.takeWhile1 Char.isAlphaNum
  tail <- Text.many' parseTail

  pure . Text.concat $ head : tail
  where
    parseTail :: Parser Text
    parseTail = do
      char      <- parseSeparator
      remaining <- Text.takeWhile1 Char.isAlphaNum

      pure $ Text.singleton char <> remaining
      where
        parseSeparator :: Parser Char
        parseSeparator
          = Text.choice
          $ Text.char <$>
          [ '-'
          , '.'
          , '_'
          , ' '
          ]

parsePadding :: Parser Text
parsePadding = Text.takeWhile1 (== ' ')

unlines :: (Foldable f, Conversion Text a) => f a -> Text
unlines = unlinesN 1

unlinesN :: (Foldable f, Conversion Text a) => Word8 -> f a -> Text
unlinesN count list
  | Foldable.null list = mempty
  -- ^ Note: that Text.intercalate returns the separator text when given an empty list
  | otherwise          = Text.intercalate emptyLines . fmap (convert @Text) $ Foldable.toList list
  where
    emptyLines :: Text
    emptyLines = Text.replicate (convertImpure $ convert @Integer count) "\n"
