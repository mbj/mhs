module PGT.Output
  ( impureParse
  , testTree
  )
where

import Data.Attoparsec.Text (Parser)
import Data.List.NonEmpty (NonEmpty(..))
import PGT.Output.Render
import PGT.Output.Test.QueryPlan (QueryStats)
import PGT.Output.TestSuite (TestSuite)
import PGT.Prelude

import qualified Data.Attoparsec.Text as Text
import qualified Data.List.NonEmpty   as NonEmpty
import qualified Data.Text            as Text hiding (take)
import qualified GHC.Err              as Err
import qualified PGT.Output.Golden    as PGT
import qualified PGT.Output.TestSuite as TestSuite
import qualified System.Path          as Path
import qualified Test.Tasty           as Tasty

newtype Document a = Document (NonEmpty (TestSuite a))
  deriving stock (Eq, Show)

instance Render (Document QueryStats) where
  render (Document outputs)
    = (`Text.snoc` '\n')
    . Text.intercalate "\n\n"
    . NonEmpty.toList $ render <$> outputs

impureParse :: Text -> Text
impureParse = either Err.error identity . parse

parse :: Text -> Either String Text
parse = fmap render . Text.parseOnly parseDocument
  where
    parseDocument :: Parser (Document QueryStats)
    parseDocument = Document . NonEmpty.fromList <$> Text.many1' TestSuite.parse

testTree :: IO Tasty.TestTree
testTree = PGT.mkDirGolden (Path.relDir "src/PGT/Output/examples") parse
