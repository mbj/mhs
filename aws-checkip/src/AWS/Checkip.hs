module AWS.Checkip (eitherReadNetAddr, printNetAddr, readNetAddr, readRequest) where

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

type NetAddr = Network.NetAddr Network.IP

printNetAddr :: Env env => MIO env ()
printNetAddr = readNetAddr >>= liftIO . IO.putStrLn . toString

readNetAddr :: Env env => MIO env NetAddr
readNetAddr = either UnliftIO.throwIO pure =<< eitherReadNetAddr

eitherReadNetAddr :: forall env . Env env => MIO env (HTTP.Result NetAddr)
eitherReadNetAddr = readRequest >>= HTTP.send decoder
  where
    parseLine :: Text -> Either HTTP.ResponseError NetAddr
    parseLine
      = maybe (Left $ HTTP.BodyDecodeFailure "cannot parse IP") (pure . mkNetAddr)
      . fromString
      . Text.unpack
      . Text.dropWhileEnd (== '\n')

    mkNetAddr :: Network.IP -> NetAddr
    mkNetAddr ip = case ip of
      Network.IPv4{} -> Network.netAddr ip 32
      Network.IPv6{} -> Network.netAddr ip 128

    decoder :: HTTP.ResponseDecoder NetAddr
    decoder
      = parseLine <=< HTTP.decodeStatus HTTP.status200 decodeBody

    decodeBody :: HTTP.ResponseDecoder Text
      = left (HTTP.BodyDecodeFailure . Text.pack . show)
      . Text.decodeUtf8' . LBS.toStrict . HTTP.responseBody

readRequest :: MonadThrow m => m HTTP.Request
readRequest = HTTP.parseRequest "https://checkip.amazonaws.com"
