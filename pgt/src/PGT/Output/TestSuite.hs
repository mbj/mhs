{-# LANGUAGE  MultiWayIf #-}

module PGT.Output.TestSuite
  ( TestSuite(..)
  , parse
  , testTree
  )
where

import Data.Attoparsec.Text (Parser)
import PGT.Output.Render
import PGT.Output.Test(Test)
import PGT.Output.Test.QueryPlan (QueryStats)
import PGT.Output.Text
import PGT.Prelude

import qualified Data.Attoparsec.Text     as Text
import qualified Data.Foldable            as Foldable
import qualified Data.Text                as Text
import qualified PGT.Output.Definition    as Definition
import qualified PGT.Output.Golden        as PGT
import qualified PGT.Output.Test          as Test
import qualified PGT.Output.Test.Result   as Result
import qualified System.Path              as Path
import qualified Test.Tasty               as Tasty

data TestSuite a = TestSuite
  { definitions :: [Text]
  , tests       :: [Test a]
  }
  deriving stock (Eq, Show)

instance Render (TestSuite QueryStats) where
  render TestSuite{..} = Text.stripEnd . unlinesN 2 $ definitions <> [renderTests]
    where
      renderTests :: Text
      renderTests = Text.stripEnd . Text.concat $ renderTest <$> tests

      renderTest :: Test QueryStats -> Text
      renderTest test = case test.result of
        Result.Error _ -> render test <> "\n\n"
        _              -> render test <> "\n\n\n"

parse :: Parser (TestSuite QueryStats)
parse = do
  definitions <- Text.many' (Definition.parse <* (Text.endOfInput <|> Text.endOfLine))
  atEnd       <- Text.atEnd

  if | atEnd && Foldable.null definitions ->
        failP "expected a valid test or database object definition but found none"
     | atEnd                              ->
        pure $ TestSuite definitions []
     | otherwise                          ->
        either failP (pure . TestSuite definitions)
          =<< Text.eitherP (parseUnexpected (listToMaybe definitions)) parseTests
  where
    parseTests :: Parser [Test QueryStats]
    parseTests = Text.many1' parseTest
      where
        parseTest :: Parser (Test QueryStats)
        parseTest = do
          test <- Test.parse

          case test.result of
            Result.Error _ -> emptyLines 1 "after an error result"
            _              -> emptyLines 2 "after a test"

          pure test
          where
            emptyLines :: Natural -> Text -> Parser ()
            emptyLines count message
              = Text.endOfInput
              <|> (eitherImpureError =<< mkParseEmptyLines count message)

    parseUnexpected :: Maybe Text -> Parser Text
    parseUnexpected definitions = parseUnexpectedEmptyLine <|> parseUnexpectedText
      where
        parseUnexpectedEmptyLine :: Parser Text
        parseUnexpectedEmptyLine = Text.endOfLine $> message
          where
            message :: Text
            message = "expected database object definition or test but found an empty line"

        parseUnexpectedText :: Parser Text
        parseUnexpectedText = do
          char <- Text.peekChar'

          case char of
            '-' -> failP "Unexpected failure! possible GHC Error"
            _   -> (mkMessage <>) <$> Text.takeText
          where
            mkMessage :: Text
            mkMessage =
              maybe
                "expected a test or database object definition but found: \n\n"
                (const "expected a test after database object definitions but found: \n\n")
                definitions

testTree :: IO Tasty.TestTree
testTree
  = PGT.mkDirGolden (Path.relDir "src/PGT/Output/TestSuite/examples")
  $ Text.parseOnly parse
