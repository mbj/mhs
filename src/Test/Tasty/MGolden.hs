module Test.Tasty.MGolden (Mode(..), goldenTest, printDetails) where

import Control.Applicative (empty, pure)
import Control.Monad ((=<<))
import Data.Bool (Bool)
import Data.Char (Char)
import Data.Eq (Eq, (==))
import Data.Foldable (traverse_)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Maybe
import Data.Ord (Ord)
import Data.Proxy (Proxy(..))
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Text (Text)
import Data.Typeable (Typeable)
import System.FilePath (FilePath)
import System.IO (IO)
import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.Providers
import Test.Tasty.Providers.ConsoleFormat
import Text.Show (Show)

import qualified Data.Algorithm.Diff as Diff
import qualified Data.Text           as Text
import qualified Data.Text.IO        as Text
import qualified System.IO.Error     as Error

data Mode = RunTest | UpdateExpected
  deriving stock (Eq, Ord, Typeable, Show)

instance IsOption Mode where
  defaultValue = RunTest
  parseValue = \case
    "test"   -> pure RunTest
    "update" -> pure UpdateExpected
    _other   -> empty

  optionName     = pure "update"
  optionHelp     = pure "Update expected on mismatched example"
  optionCLParser = flagCLParser empty UpdateExpected

data Golden = Golden
  { action       :: IO Text
  , expectedPath :: FilePath
  }
  deriving stock Typeable

instance IsTest Golden where
  run options golden _callback = runGolden golden options
  testOptions = pure . pure $ Option (Proxy :: Proxy Mode)

goldenTest :: String -> FilePath -> IO Text -> TestTree
goldenTest name expectedPath action = singleTest name Golden{..}

runGolden :: Golden -> OptionSet -> IO Result
runGolden golden@Golden{..} options = do
  actual <- action

  maybe
    (absentFile golden options actual)
    (testExpected golden options actual)
    =<< tryRead expectedPath

absentFile :: Golden -> OptionSet -> Text -> IO Result
absentFile golden@Golden{..} options actual =
  if shouldUpdate options
    then updateExpected golden actual
    else pure $ testFailed "file is absent"

testExpected :: Golden -> OptionSet -> Text -> Text -> IO Result
testExpected golden@Golden{..} options actual expected =
  if expected == actual
    then pure $ testPassed empty
    else mismatch options golden expected actual

mismatch :: OptionSet -> Golden -> Text -> Text -> IO Result
mismatch options golden expected actual =
  if shouldUpdate options
    then updateExpected golden actual
    else pure . testFailedDetails empty $ printDetails Text.putStrLn expected actual

updateExpected :: Golden -> Text -> IO Result
updateExpected Golden{..} actual = do
  Text.writeFile expectedPath actual
  pure $ testPassed "UPDATE"

printDetails :: (Text -> IO ()) -> Text -> Text -> ResultDetailsPrinter
printDetails putStrLn expected actual = ResultDetailsPrinter print
  where
    print :: Int -> (ConsoleFormat -> IO () -> IO ()) -> IO ()
    print _indent formatter
      = traverse_ printDiff
      $ Diff.getGroupedDiff (Text.lines actual) (Text.lines expected)
      where
        printDiff :: Diff.Diff [Text] -> IO ()
        printDiff = \case
          (Diff.Both   line _) -> printLines ' ' neutralFormat line
          (Diff.First  line)   -> printLines '+' addFormat line
          (Diff.Second line)   -> printLines '-' removeFormat line

        printLines :: Char -> ConsoleFormat -> [Text] -> IO ()
        printLines prefix format lines
          = formatter format
          $ traverse_ printLine lines
          where
            printLine :: Text -> IO ()
            printLine line = putStrLn $ Text.singleton prefix <> line

addFormat :: ConsoleFormat
addFormat = okFormat

neutralFormat :: ConsoleFormat
neutralFormat = infoOkFormat

removeFormat :: ConsoleFormat
removeFormat = infoFailFormat

shouldUpdate :: OptionSet -> Bool
shouldUpdate options = (lookupOption options :: Mode) == UpdateExpected

tryRead :: FilePath -> IO (Maybe Text)
tryRead path =
  Error.catchIOError
    (pure <$> Text.readFile path)
    handler
  where
    handler :: Error.IOError -> IO (Maybe Text)
    handler error =
      if Error.isDoesNotExistError error
        then pure empty
        else Error.ioError error
