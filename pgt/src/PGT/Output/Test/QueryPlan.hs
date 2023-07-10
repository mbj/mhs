module PGT.Output.Test.QueryPlan
  ( QueryPlanJson(..)
  , QueryStats(..)
  , mkQueryStats
  , parse
  , testTree
  )
where

import Data.Attoparsec.Text (Parser)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Scientific (Scientific)
import PGT.Output.Render
import PGT.Output.Text
import PGT.Prelude
import Prelude (floor)

import qualified Data.Aeson           as JSON
import qualified Data.Attoparsec.Text as Text
import qualified Data.Char            as Char
import qualified Data.List.NonEmpty   as NonEmpty
import qualified Data.Text            as Text hiding (take)
import qualified Data.Text.Encoding   as Text
import qualified PGT.Output.Golden    as PGT
import qualified System.Path          as Path
import qualified Test.Tasty           as Tasty

data QueryPlanJson = QueryPlanJson
  { executionTime :: Scientific
  , plan          :: JSON.Object
  , planning      :: JSON.Object
  , planningTime  :: Scientific
  , triggers      :: Vector JSON.Value
  }
  deriving stock (Eq, Generic, Show)

instance JSON.FromJSON QueryPlanJson where
  parseJSON = JSON.genericParseJSON jsonOptions
    where
      jsonOptions :: JSON.Options
      jsonOptions = JSON.defaultOptions
        { JSON.fieldLabelModifier  = convert . Text.toTitle . convert . JSON.camelTo2 ' '
        , JSON.rejectUnknownFields = True
        }

data TimeQuantity
  = Magnitude  Natural
  | RealMsTime Scientific
  deriving stock (Eq, Show)

data QueryStats = QueryStats
  { planningTime  :: TimeQuantity
  , executionTime :: TimeQuantity
  }
  deriving stock (Eq, Show)

instance Render QueryStats where
  render QueryStats{..} = unlines stats
    where
      stats :: [Text]
      stats =
        [ "Query plan signals\n"
        , "PlanningTime:  " <> showc planningTime
        , "ExecutionTime: " <> showc executionTime
        ]

parse :: Parser QueryPlanJson
parse = do
  void $ parsePadding *> "QUERY PLAN" *> Text.takeWhile1 Char.isSpace

  Text.takeWhile1 (== '-') *> Text.endOfLine

  jsonLines <- NonEmpty.fromList <$> Text.manyTill' parseLineChars (Text.string termination)

  initJsonLines <- traverse stripTrailingPlus $ NonEmpty.init jsonLines

  let json = unlines $ initJsonLines <> [NonEmpty.last jsonLines]

  either (errorP . convert) (pure . NonEmpty.head)
    $ JSON.eitherDecode @(NonEmpty QueryPlanJson)  (convert $ Text.encodeUtf8 json)
  where
    termination :: Text
    termination = "(1 row)"

    stripTrailingPlus :: Text -> Parser Text
    stripTrailingPlus text =
      maybe
        (errorP $ "expected more than one json char but found: " <> text)
        stripPlusParser
      $ Text.unsnoc text
      where
        stripPlusParser :: (Text, Char) -> Parser Text
        stripPlusParser (init, char)
          | char == '+' =
              pure init
          | otherwise   =
              errorP $ "expected a '+' char at the end of the json line but found " <> Text.singleton char

mkQueryStats :: QueryPlanJson -> QueryStats
mkQueryStats QueryPlanJson{..} =
  QueryStats
    { planningTime  = renderTime planningTime
    , executionTime = renderTime executionTime
    }
  where
    renderTime :: Scientific -> TimeQuantity
    renderTime value
      | natural > 1500 = RealMsTime value
      | natural > 1000 = Magnitude 3
      | natural > 500  = Magnitude 2
      | natural > 250  = Magnitude 1
      | otherwise      = Magnitude 0
      where
        natural :: Natural
        natural = floor (value * 1e3)

testTree :: IO Tasty.TestTree
testTree
  = PGT.mkDirGolden (Path.relDir "src/PGT/Output/Test/QueryPlan/examples")
  $ Text.parseOnly (mkQueryStats <$> parse)
