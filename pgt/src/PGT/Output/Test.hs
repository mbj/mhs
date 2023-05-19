{-# LANGUAGE DeriveFunctor #-}

module PGT.Output.Test
  ( Test
  , Test'(..)
  , parse
  , testTree
  )
where

import Data.Attoparsec.Text (Parser)
import Data.Maybe (maybeToList)
import Data.Word (Word8)
import PGT.Output.Render
import PGT.Output.RowCount (RowCount(..))
import PGT.Output.Test.Comments (Comment, Commentary(..), Comments, MetaComment(..))
import PGT.Output.Test.QueryPlan (QueryStats)
import PGT.Output.Test.Result (Result(..))
import PGT.Output.Text
import PGT.Prelude

import qualified Data.Attoparsec.Text      as Text
import qualified PGT.Output.Golden         as PGT
import qualified PGT.Output.Test.Comments  as Comments
import qualified PGT.Output.Test.QueryPlan as QueryPlan
import qualified PGT.Output.Test.Result    as Result
import qualified System.Path               as Path
import qualified Test.Tasty                as Tasty

data Test' commentary queryPlan = Test
  { alterLogs  :: [Text]
  , commentary :: commentary
  , queryPlan  :: Maybe queryPlan
  , result     :: Result
  , subTests   :: [Test' Comment queryPlan]
  }
  deriving stock (Eq, Functor, Show)

type Test queryPlan = Test' Comments queryPlan

instance (Render a) => Render (Test a) where
  render = go 2
    where
      go :: forall b c . (Render b, Render c) => Word8 -> Test' b c -> Text
      go count Test{..}
        = unlinesN count
        $ [ render commentary
          , logsWithResults
          ]
        <> fmap render (maybeToList queryPlan)
        <> (go 1 <$> subTests)
        where
          logsWithResults :: Text
          logsWithResults = case alterLogs of
            [] -> render result
            _  -> unlines @[] [unlines alterLogs, render result]

parse :: Parser (Test QueryStats)
parse = do
  test     <- mkParser (Comments.parseComments <* impureParseEmptyLine "after test comments")
  subTests <- Text.many' (Text.endOfLine *> mkParser Comments.parseComment)

  pure $ test{ subTests = subTests }
  where
    mkParser :: Parser (Commentary a) -> Parser (Test' (Commentary a) QueryStats)
    mkParser commentsParser = do
      commentary <- commentsParser
      alterLogs  <- parseAlterLogs
      result     <- Result.parse
      queryPlan  <- optional (parseEmptyLine "before a query plan" *> QueryPlan.parse)

      fmap QueryPlan.mkQueryStats <$> validate Test{ subTests = [], .. }

parseAlterLogs :: Parser [Text]
parseAlterLogs = Text.many' parseLog
  where
    parseLog :: Parser Text
    parseLog = do
      prefix <- Text.choice $ Text.string <$> logPrefixes

      (prefix <>) <$> parseLineChars

    logPrefixes :: [Text]
    logPrefixes
      = [ "NOTICE:"
        , "DETAIL:"
        , "drop"
        ]

validate :: Test' (Commentary a) b -> Parser (Test' (Commentary a) b)
validate test
  = either
    errorP
    (const (pure test))
  $ validateTest test
  where
    validateTest :: Test' (Commentary a) b -> Either Text ()
    validateTest Test{commentary = Commentary{..}, ..} =
      case metaComment of
        ErrorMetaComment             -> assertErrorResult
        RowCountMetaComment rowCount -> assertRowCount rowCount
        _                            -> assertRowCount (RowCount 1)
      where
        assertErrorResult :: Either Text ()
        assertErrorResult = case result of
          Error _ -> pure ()
          _       -> Left . ("expected an error result but found:\n\n" <>) . convert $ render result

        assertRowCount :: RowCount -> Either Text ()
        assertRowCount commentRowCount = case result of
          Empty           -> assertCount $ RowCount 0
          Error _         -> Left . ("expected a row result but found:\n\n" <>) . convert $ render result
          Records records -> assertCount $ Result.itemsRowCount records
          Rows    rows    -> assertCount $ rows.rowCount
          Tuples  tuples  -> assertCount $ Result.itemsRowCount tuples
          where
            assertCount :: RowCount -> Either Text ()
            assertCount itemsRowCount =
              when (itemsRowCount /= commentRowCount)
                . Left
                . convert
                $ "expected " <> render itemsRowCount <> " comment but received " <> render commentRowCount

testTree :: IO Tasty.TestTree
testTree
  = PGT.mkDirGolden (Path.relDir "src/PGT/Output/Test/examples")
  $ Text.parseOnly parse
