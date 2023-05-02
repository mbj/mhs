module PGT.Output.Golden (mkDirGolden) where

import Control.Exception (ErrorCall, catch, evaluate)
import Data.Tuple (fst)
import PGT.Output.Render
import PGT.Prelude
import System.Path ((</>))

import qualified Data.ByteString       as BS
import qualified Data.List             as List
import qualified Data.Text             as Text
import qualified Data.Text.Encoding    as Text
import qualified System.Path           as Path
import qualified System.Path.Directory as Path
import qualified Test.Tasty            as Tasty
import qualified Test.Tasty.MGolden    as Tasty.MGolden

type Parser a = Text -> Either String a

mkDirGolden :: Render a => Path.RelDir -> Parser a -> IO Tasty.TestTree
mkDirGolden directory parser =
  Tasty.testGroup (Path.toString directory) . fmap mkGolden
    <$> getInputFiles
  where
    mkGolden :: Path.RelFile -> Tasty.TestTree
    mkGolden path
      = Tasty.MGolden.goldenTest
          (Path.toString path)
          (Path.toString outputFile)
          runTest
      where
        runTest :: IO Text
        runTest = do
          content <- Text.decodeUtf8 <$> BS.readFile (Path.toString inputFile)

          either (pure . removeDynamicText . convert) (pure . render)
            =<< catch @ErrorCall (evaluate $ parser content) (pure . Left . show)
          where
            removeDynamicText :: Text -> Text
            removeDynamicText = fst . Text.breakOn "CallStack (from HasCallStack)"

        outputFile :: Path.RelFile
        outputFile = Path.addExtension inputFile ".parsed"

        inputFile :: Path.RelFile
        inputFile = directory </> path

    getInputFiles :: IO [Path.RelFile]
    getInputFiles =
      List.filter isCandidate <$> Path.filesInDir directory
      where
        isCandidate :: Path.RelFile -> Bool
        isCandidate = List.isSuffixOf ".expected" . Path.toString
