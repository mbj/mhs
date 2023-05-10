{-# LANGUAGE DeriveFunctor #-}

module PGT.Output.Test
  ( Test(..)
  , parse
  , testTree
  )
where

import Data.Attoparsec.Text (Parser)
import Data.Maybe (maybeToList)
import PGT.Output.Render
import PGT.Output.RowCount (RowCount(..))
import PGT.Output.Test.Comments (Commentary(..), Comments, MetaComment(..))
import PGT.Output.Test.QueryPlan (QueryStats)
import PGT.Output.Test.Result (Result(..))
import PGT.Output.Text
import PGT.Prelude

import qualified Data.Attoparsec.Text      as Text
import qualified GHC.Err                   as Err
import qualified PGT.Output.Golden         as PGT
import qualified PGT.Output.Test.Comments  as Comments
import qualified PGT.Output.Test.QueryPlan as QueryPlan
import qualified PGT.Output.Test.Result    as Result
import qualified System.Path               as Path
import qualified Test.Tasty                as Tasty

data Test a = Test
  { comments  :: Comments
  , queryPlan :: Maybe a
  , result    :: Result
  }
  deriving stock (Eq, Functor, Show)

instance Render a => Render (Test a) where
  render Test{..}
    = unlinesN 2
    $ [ render comments
      , render result
      ]
      <> fmap render (maybeToList queryPlan)

parse :: Parser (Test QueryStats)
parse = do
  comments  <- Comments.parse <* impureParseEmptyLine "after test comments"
  result    <- Result.parse
  queryPlan <- optional (parseEmptyLine "before a query plan" *> QueryPlan.parse)

  fmap QueryPlan.mkQueryStats <$> validate Test{..}

validate :: Test a -> Parser (Test a)
validate test
  = either
    Err.error
    (const (pure test))
  $ validateTest test
  where
    validateTest :: Test a -> Either String ()
    validateTest Test{comments = Commentary{..}, ..} =
      case metaComment of
        ErrorMetaComment             -> assertErrorResult
        RowCountMetaComment rowCount -> assertRowCount rowCount
      where
        assertErrorResult :: Either String ()
        assertErrorResult = case result of
          Error _ -> pure ()
          _       -> Left . ("expected an error result but found:\n\n" <>) . convert $ render result

        assertRowCount :: RowCount -> Either String ()
        assertRowCount commentRowCount = case result of
          Records records -> assertCount $ Result.recordsCount records
          Rows    rows    -> assertCount $ rows.rowCount
          Empty           -> assertCount $ RowCount 0
          Error _         -> Left . ("expected a row result but found:\n\n" <>) . convert $ render result
          where
            assertCount :: RowCount -> Either String ()
            assertCount itemsRowCount =
              when (itemsRowCount /= commentRowCount)
                . Left
                . convert
                $ "expected " <> render itemsRowCount <> " comment but received " <> render commentRowCount

testTree :: IO Tasty.TestTree
testTree
  = PGT.mkDirGolden (Path.relDir "src/PGT/Output/Test/examples")
  $ Text.parseOnly parse
