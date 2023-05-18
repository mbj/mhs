module PGT.Output.Definition
  ( parse
  , testTree
  )
where

import Control.Monad (replicateM)
import Data.Attoparsec.Text (Parser)
import Data.Int (Int)
import Data.Maybe (maybeToList)
import PGT.Output.Text
import PGT.Prelude

import qualified Data.Attoparsec.Text as Text
import qualified Data.Char            as Char
import qualified Data.Text            as Text
import qualified PGT.Output.Golden    as PGT
import qualified System.Path          as Path
import qualified Test.Tasty           as Tasty

data DefinitionType
  = CompositeType
  | Domains
  | Functions
  | View

instance Conversion Text DefinitionType where
  convert = \case
    CompositeType -> "Composite type"
    Domains       -> "List of domains"
    Functions     -> "List of functions"
    View          -> "View"

parse :: Parser Text
parse = Text.choice
  [ mkParser CompositeType
  , mkParser Domains
  , mkParser Functions
  , mkParser View
  ]
  where
    mkParser :: DefinitionType -> Parser Text
    mkParser definitionType = do
      padding         <- optional parsePadding
      title           <- parseTitle
      subTitles       <- parseLineChars
      underLine       <- parseLineChars
      definitionLines <- Text.many1' parseDefinitionLine
      termination     <- parseTermination

      let title' = maybe title (<> title) padding

      pure
        .  unlines
        $  [title', subTitles, underLine]
        <> definitionLines
        <> termination
      where
        parseDefinitionLine :: Parser Text
        parseDefinitionLine = case definitionType of
          CompositeType -> parseLine 2
          Domains       -> parseLine 3
          Functions     -> parseLine 3
          View          -> parseLine 2
          where
            parseLine :: Int -> Parser Text
            parseLine count = do
              required  <- Text.concat <$> replicateM count parseRequiredValue
              optional' <- parseLineChars

              pure $ required <> optional'
              where
                parseRequiredValue :: Parser Text
                parseRequiredValue = do
                  value <- Text.space *> definitionName <* Text.char '|'

                  pure $ " " <> value <> "|"

                definitionName :: Parser Text
                definitionName = Text.takeWhile1 isDefinitionName
                  where
                    isDefinitionName char = Char.isPrint char && char /= '|'

        parseTermination :: Parser [Text]
        parseTermination =
          maybeToList <$> optional (Text.string "(1 row)" <* Text.endOfLine)

        parseTitle :: Parser Text
        parseTitle = case definitionType of
          CompositeType -> parseWithTail
          View          -> parseWithTail
          _             -> Text.string name <* Text.endOfLine
          where
            name :: Text
            name = convert definitionType

            parseWithTail :: Parser Text
            parseWithTail = do
              tail <- Text.string name *> parseLineChars
              pure $ name <> tail

testTree :: IO Tasty.TestTree
testTree
  = PGT.mkDirGolden (Path.relDir "src/PGT/Output/Definition/examples")
  $ Text.parseOnly parse
