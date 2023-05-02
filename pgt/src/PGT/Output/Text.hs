module PGT.Output.Text
  ( parseLineChars
  , parseName
  )
where

import Data.Attoparsec.Text (Parser)
import PGT.Prelude

import qualified Data.Attoparsec.Text as Text
import qualified Data.Char            as Char
import qualified Data.Text            as Text

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
