module Main where

import AWS.Lambda.ALB
import AWS.Prelude

import qualified System.IO  as IO

handler :: Request Text -> RIO LambdaEnv Response
handler event = do
  liftIO $ IO.hPutStr IO.stderr "Lambda Event started"

  pure $ Response
    { isBase64Encoded = False
    , statusCode      = 200
    , headers         = Headers [("content-type", "application/json")]
    , body            = "hello world at " <> path event
    }

main :: IO ()
main = do
  env <- either throwM pure =<< getLambdaEnv
  runALB env handler
