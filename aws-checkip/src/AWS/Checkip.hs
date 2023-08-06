module AWS.Checkip (eitherReadIP, printIP, readIP, readRequest) where

import AWS.Checkip.Prelude
import Control.Arrow (left)
import Control.Monad.Catch (MonadThrow)
import Data.Textual (fromString, toString)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as Text
import qualified Network.HTTP.Client  as HTTP
import qualified Network.HTTP.MClient as HTTP
import qualified Network.HTTP.Types   as HTTP
import qualified Network.IP.Addr      as Network
import qualified System.IO            as IO
import qualified UnliftIO.Exception   as UnliftIO

type Env env = (HasCallStack, HTTP.Env env)

printIP :: Env env => MIO env ()
printIP = readIP >>= liftIO . IO.putStrLn . toString

readIP :: Env env => MIO env Network.IP
readIP = either UnliftIO.throwIO pure =<< eitherReadIP

eitherReadIP :: forall env . Env env => MIO env (HTTP.Result Network.IP)
eitherReadIP = readRequest >>= HTTP.send decoder
  where
    parseLine :: Text -> Either HTTP.ResponseError Network.IP
    parseLine
      = maybe (Left $ HTTP.BodyDecodeFailure "cannot parse IP") pure
      . fromString
      . Text.unpack
      . Text.dropWhileEnd (== '\n')

    decoder :: HTTP.ResponseDecoder Network.IP
    decoder
      = parseLine <=< HTTP.decodeStatus HTTP.status200 decodeBody

    decodeBody :: HTTP.ResponseDecoder Text
      = left (HTTP.BodyDecodeFailure . Text.pack . show)
      . Text.decodeUtf8' . LBS.toStrict . HTTP.responseBody

readRequest :: MonadThrow m => m HTTP.Request
readRequest = HTTP.parseRequest "https://checkip.amazonaws.com"
