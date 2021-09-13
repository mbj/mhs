-- | Lambda CLI example command:
--
-- cli-example -e '{"name": "allan"}' -f output.json
--
module Main where

import AWS.Prelude
import Data.String (fromString)
import Options.Applicative

import qualified Data.Aeson                 as JSON
import qualified Data.Aeson.Encode.Pretty   as JSON
import qualified Data.ByteString.Lazy       as BS
import qualified Data.ByteString.Lazy.Char8 as BSC

main :: IO ()
main = run pure

data OutputLocation
  = FileOutput String
  | StdOutput

data LambdaOptions = LambdaOptions
  { event          :: String
  , outputLocation :: OutputLocation
  }

run
  :: forall m . MonadIO m
  => (JSON.Value -> m JSON.Value)
  -> m ()
run lambdaFn = do
  LambdaOptions {..} <- liftIO $ execParser parserInfo
  jsonEvent          <- either error pure . JSON.eitherDecode $ fromString event
  output             <- JSON.encodePretty <$> invokeLambdaFunction jsonEvent lambdaFn
  liftIO $ case outputLocation of
    FileOutput filePath -> BS.writeFile filePath output
    StdOutput           -> BSC.putStrLn output

invokeLambdaFunction
  :: forall m . JSON.Value
  -> (JSON.Value -> m JSON.Value)
  -> m JSON.Value
invokeLambdaFunction eventJSON lambdaFn = lambdaFn eventJSON

parserInfo :: ParserInfo LambdaOptions
parserInfo = wrapHelper parser "Invoke lambda handler function with a JSON event"
  where
    parser :: Parser LambdaOptions
    parser = LambdaOptions <$> eventParser <*> outputParser

eventParser :: Parser String
eventParser = strOption
  (  long "event"
  <> short 'e'
  <> metavar "EventJSON"
  <> help "Lambda event json"
  )

outputParser :: Parser OutputLocation
outputParser = fileOutput <|> pure StdOutput
  where
    fileOutput :: Parser OutputLocation
    fileOutput = FileOutput <$> strOption
      (  long "file"
      <> short 'f'
      <> metavar "FILENAME"
      <> help "output file"
      )

wrapHelper :: Parser b -> String -> ParserInfo b
wrapHelper parser desc = info parser $ progDesc desc
