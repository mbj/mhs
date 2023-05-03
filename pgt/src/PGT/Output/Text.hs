module PGT.Output.Text
  ( impureParseEmptyLine
  , mkParseEmptyLines
  , parseEmptyLine
  , parseLineChars
  , parseName
  , parsePadding
  , unlines
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
unlines = Text.intercalate "\n" . fmap (convert @Text) . Foldable.toList
