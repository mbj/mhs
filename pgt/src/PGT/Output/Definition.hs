module PGT.Output.Definition
  ( parse
  , testTree
  )
where

import Data.Attoparsec.Text (Parser)
import PGT.Output.Text
import PGT.Prelude

import qualified Data.Attoparsec.Text as Text
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
      definitionLines <- Text.many1' parseLineChars

      let title' = maybe title (<> title) padding

      pure
        .  unlines
        $  [title', subTitles, underLine]
        <> definitionLines
      where
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
