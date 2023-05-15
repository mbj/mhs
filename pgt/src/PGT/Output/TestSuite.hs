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

import qualified Data.Attoparsec.Text  as Text
import qualified Data.Foldable         as Foldable
import qualified Data.Text             as Text
import qualified GHC.Err               as Err
import qualified PGT.Output.Definition as Definition
import qualified PGT.Output.Golden     as PGT
import qualified PGT.Output.Test       as Test
import qualified System.Path           as Path
import qualified Test.Tasty            as Tasty

data TestSuite a = TestSuite
  { definitions :: [Text]
  , tests       :: [Test a]
  }
  deriving stock (Eq, Show)

instance Render (TestSuite QueryStats) where
  render TestSuite{..} = Text.stripEnd . unlinesN 2 $ definitions <> [renderTests]
    where
      renderTests :: Text
      renderTests = unlinesN 3 . Foldable.toList $ render <$> tests

parse :: Parser (TestSuite QueryStats)
parse = do
  definitions <- Text.many' (Definition.parse <* (Text.endOfInput <|> Text.endOfLine))
  atEnd       <- Text.atEnd

  if | atEnd && Foldable.null definitions ->
        fail "expected a valid test or database object definition but found none"
     | atEnd                              ->
        pure $ TestSuite definitions []
     | otherwise                          ->
        either fail (pure . TestSuite definitions)
          =<< Text.eitherP (parseUnexpected (listToMaybe definitions)) parseTests
  where
    parseTests :: Parser [Test QueryStats]
    parseTests = Text.many1' (Test.parse <* (Text.endOfInput <|> emptyLines))
      where
        emptyLines :: Parser ()
        emptyLines = either Err.error pure =<< mkParseEmptyLines 2 "after a test"

    parseUnexpected :: Maybe Text -> Parser String
    parseUnexpected definitions = parseUnexpectedEmptyLine <|> parseUnexpectedText
      where
        parseUnexpectedEmptyLine :: Parser String
        parseUnexpectedEmptyLine = Text.endOfLine $> message
          where
            message :: String
            message = "expected database object definition or test but found an empty line"

        parseUnexpectedText :: Parser String
        parseUnexpectedText = do
          char <- Text.peekChar'

          case char of
            '-' -> fail "Unexpected failure! possible GHC Error"
            _   -> (mkMessage <>) . convert <$> Text.takeText
          where
            mkMessage :: String
            mkMessage =
              maybe
                "expected a test or database object definition but found: \n\n"
                (const "expected a test after database object definitions but found: \n\n")
                definitions

testTree :: IO Tasty.TestTree
testTree
  = PGT.mkDirGolden (Path.relDir "src/PGT/Output/TestSuite/examples")
  $ Text.parseOnly parse
