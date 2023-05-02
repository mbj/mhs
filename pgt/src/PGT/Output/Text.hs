module PGT.Output.Text
  ( impureParseEmptyLine
  , parseEmptyLine
  , parseLineChars
  , parseName
  )
where

import Data.Attoparsec.Text (Parser)
import PGT.Prelude

import qualified Data.Attoparsec.Text as Text
import qualified Data.Char            as Char
import qualified Data.Foldable        as Foldable
import qualified Data.Text            as Text
import qualified GHC.Err              as Err

impureParseEmptyLine :: String -> Parser ()
impureParseEmptyLine = either Err.error pure <=< mkParseEmptyLine

parseEmptyLine :: String -> Parser ()
parseEmptyLine = either fail pure <=< mkParseEmptyLine

mkParseEmptyLine :: String -> Parser (Either String ())
mkParseEmptyLine message = do
  emptyLinesCount <- Foldable.length <$> Text.many' Text.endOfLine

  if emptyLinesCount == 1
    then pure $ pure ()
    else pure . Left $ "found " <> show emptyLinesCount <> " empty lines " <> message <> " instead of 1"

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
