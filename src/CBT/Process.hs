module CBT.Process (Proc, captureText) where

import CBT.Prelude

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as Text
import qualified System.Process.Typed as Process

type Proc = Process.ProcessConfig () () ()

captureText
  :: MonadIO m
  => Process.ProcessConfig stdin stdout stderr
  -> m Text
captureText proc
  =   Text.strip
  .   Text.decodeUtf8
  .   LBS.toStrict
  <$> Process.readProcessStdout_ proc
