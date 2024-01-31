{-# LANGUAGE CPP #-}

module Main (main) where

import AWS.Checkip.Prelude

import qualified AWS.Checkip
import qualified Data.ByteString.Lazy         as LBS
import qualified Devtools
import qualified MIO.Log                      as Log
import qualified Network.HTTP.Client          as HTTP
import qualified Network.HTTP.Client.Internal as HTTP
import qualified Network.HTTP.MClient         as HTTP
import qualified Network.HTTP.Types           as HTTP
import qualified Network.IP.Addr              as Network
import qualified Test.Tasty                   as Tasty
import qualified Test.Tasty.HUnit             as Tasty

data Environment = Environment
  { httpSendRequest :: HTTP.SendRequest
  , logAction       :: Log.Action
  }

main :: IO ()
main = do
  request <- AWS.Checkip.readRequest
  liftIO . Tasty.defaultMain .
    Tasty.testGroup "aws-checkip" $
      [ Devtools.testTree $$(Devtools.readDependencies [Devtools.Target "aws-checkip"])
      , testHTTP request
      ]

testHTTP :: HTTP.Request -> Tasty.TestTree
testHTTP request
  = Tasty.testGroup "http"
  [ testResponse
      "invalid body"
      (mkSuccessResponse "")
      (Left $ HTTP.BodyDecodeFailure "cannot parse IP")
  , testResponse
      "valid ipv4 body"
      (mkSuccessResponse "127.0.0.1\n")
      (pure $ Network.netAddr (Network.IPv4 Network.loopbackIP4) 32)
  , testResponse
      "valid ipv6 body"
      (mkSuccessResponse "::1\n")
      (pure $ Network.netAddr (Network.IPv6 Network.loopbackIP6) 128)
  , testResponse
      "non 2xx"
      (mkResponse HTTP.status500 "some random body")
      (Left $ HTTP.UnexpectedStatusCode HTTP.status500)
  ]
  where
    testResponse
      :: String
      -> HTTP.Response LBS.ByteString
      -> Either HTTP.ResponseError (Network.NetAddr Network.IP)
      -> Tasty.TestTree
    testResponse name response expected = Tasty.testCase name $ do
      Tasty.assertEqual "" (show expected) =<<
        runMIO
          Environment
          { httpSendRequest = const $ pure response
          , logAction       = Log.noopAction
          }
        (show <$> AWS.Checkip.eitherReadNetAddr)

    mkResponse :: HTTP.Status -> LBS.ByteString -> HTTP.Response LBS.ByteString
    mkResponse status body
      = HTTP.Response
      { responseBody            = body
      , responseClose'          = HTTP.ResponseClose $ pure ()
      , responseCookieJar       = HTTP.CJ []
#if MIN_VERSION_http_client(0,7,16)
      , responseEarlyHints      = []
#endif
      , responseHeaders         = []
      , responseOriginalRequest = request
      , responseStatus          = status
      , responseVersion         = HTTP.http11
      }

    mkSuccessResponse :: LBS.ByteString -> HTTP.Response LBS.ByteString
    mkSuccessResponse = mkResponse HTTP.status200
