module PGT.Output
  ( impureParse
  , testTree
  )
where

import Data.Attoparsec.Text (Parser)
import Data.List.NonEmpty (NonEmpty(..))
import PGT.Output.Render
import PGT.Output.Test (Test, Test'(..))
import PGT.Output.Test.QueryPlan (QueryStats)
import PGT.Output.TestSuite (TestSuite)
import PGT.Prelude

import qualified Data.Attoparsec.Text   as Text
import qualified Data.List              as List
import qualified Data.List.NonEmpty     as NonEmpty
import qualified Data.Text              as Text
import qualified GHC.Err                as Err
import qualified PGT.Output.Golden      as PGT
import qualified PGT.Output.Test.Result as Result
import qualified PGT.Output.TestSuite   as TestSuite
import qualified System.Path            as Path
import qualified Test.Tasty             as Tasty

newtype Document a = Document (NonEmpty (TestSuite a))
  deriving stock (Eq, Show)

instance Render (Document QueryStats) where
  render (Document outputs)
    = (`Text.snoc` '\n')
    . Text.stripEnd
    . Text.concat
    $ renderTestSuite <$> NonEmpty.toList outputs
    where
      renderTestSuite :: TestSuite QueryStats -> Text
      renderTestSuite testSuite = case testSuite.tests of
        [] -> render testSuite
        _  -> render testSuite <> appendNewLineSeparator (List.last testSuite.tests)
        where
          appendNewLineSeparator :: Test QueryStats -> Text
          appendNewLineSeparator test = case test.result of
            Result.Error _ -> "\n\n"
            _              -> "\n\n\n"

impureParse :: Text -> Text
impureParse = either Err.error identity . parse

parse :: Text -> Either String Text
parse = fmap render . Text.parseOnly parseDocument
  where
    parseDocument :: Parser (Document QueryStats)
    parseDocument = Document . NonEmpty.fromList <$> Text.many1' TestSuite.parse

testTree :: IO Tasty.TestTree
testTree = PGT.mkDirGolden (Path.relDir "src/PGT/Output/examples") parse
