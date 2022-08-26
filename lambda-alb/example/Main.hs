module Main where

import AWS.Lambda.ALB
import AWS.Lambda.Runtime.Prelude

import qualified Network.HTTP.Types as HTTP
import qualified System.IO          as IO

handler :: Request -> IO Response
handler event = do
  liftIO $ IO.hPutStr IO.stderr "Lambda Event started"

  pure $ Response
    { statusCode = HTTP.status200
    , headers    = Headers [("content-type", "application/json")]
    , body       = mkTextResponseBody $ "hello world at " <> path event
    }

main :: IO ()
main = run handler
