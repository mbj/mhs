module Main where

import AWS.Lambda.ALB
import AWS.Lambda.Runtime.Prelude

import qualified Network.HTTP.Types as HTTP
import qualified System.IO          as IO

handler :: Request Text -> IO Response
handler event = do
  liftIO $ IO.hPutStr IO.stderr "Lambda Event started"

  pure $ Response
    { body       = mkTextResponseBody $ "hello world at " <> path event
    , headers    = Headers [("content-type", "application/json")]
    , statusCode = HTTP.status200
    }

main :: IO ()
main = run handler
