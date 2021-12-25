-- | XRay daemon connectivity
--
-- Only supports localhost for now.
--
-- Implements the UDP transport for the daemon protocol:
-- see https://github.com/aws/aws-xray-daemon#sending-segment-documents
module XRay.Connection
  ( SendSegment
  , connectDaemon
  , connectNull
  , renderSegment
  )
where

import Control.Applicative ((<*))
import Data.Binary.Builder (fromByteString, toLazyByteString)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (singleton)
import Data.Word (Word8)
import Network.Socket.ByteString (sendAll)
import Prelude(Integral, fromIntegral)
import XRay.Prelude
import XRay.Segment

import qualified Data.Aeson.Encoding  as JSON
import qualified Data.Aeson.Types     as JSON
import qualified Data.Attoparsec.Text as Text
import qualified Network.Socket       as Socket
import qualified UnliftIO.Exception   as Exception

type SendSegment = Segment -> IO ()

connectNull :: Segment -> IO ()
connectNull = const $ pure ()

connectDaemon :: Text -> IO (Segment -> IO ())
connectDaemon = connect <=< parseSocketAddress

parseSocketAddress :: MonadUnliftIO m => Text -> m Socket.SockAddr
parseSocketAddress input
  = maybe (Exception.throwString $ "Invalid address format: " <> show input) pure
  $ parseMaybe sockAddrParser input

connect :: Socket.SockAddr -> IO (Segment -> IO ())
connect address = do
  socket <- liftIO $ Socket.socket Socket.AF_INET Socket.Datagram Socket.defaultProtocol
  liftIO $ Socket.connect socket address
  pure (liftIO . sendAll socket . renderSegment)

renderSegment :: Segment -> ByteString
renderSegment segment = convert . toLazyByteString $ builder
  where
    builder
      =  JSON.fromEncoding format
      <> fromByteString (singleton '\n')
      <> JSON.fromEncoding (JSON.toEncoding segment)

format :: JSON.Encoding
format = JSON.value . JSON.Object $
  [ ("format",  JSON.toJSON ("json" :: Text))
  , ("version", JSON.toJSON (1 :: Natural))
  ]

portNumberParser :: Text.Parser Socket.PortNumber
portNumberParser = parseMaxBound @Socket.PortNumber

hostAddressParser :: Text.Parser Socket.HostAddress
hostAddressParser
  =   Socket.tupleToHostAddress
  <$> ((,,,) <$> (word8 <* ".") <*> (word8 <* ".") <*> (word8 <* ".") <*> word8)
  where
    word8 = parseMaxBound @Word8

parseMaxBound :: forall a . (Bounded a, Integral a) => Text.Parser a
parseMaxBound = do
  value <- Text.decimal @Natural

  if value > fromIntegral (maxBound :: a)
    then fail "Invalid decimal bounds"
    else pure (fromIntegral value)

parseMaybe :: Text.Parser a -> Text -> Maybe a
parseMaybe parser
  = either (const empty) pure
  . Text.parseOnly (parser <* Text.endOfInput)

sockAddrParser :: Text.Parser Socket.SockAddr
sockAddrParser = do
  hostAddress <- hostAddressParser
  void ":"
  portNumber <- portNumberParser
  pure $ Socket.SockAddrInet portNumber hostAddress
