module CBT.Proc where

import CBT.Backend
import CBT.Config
import CBT.Prelude

import qualified Data.ByteString.Lazy  as LBS
import qualified Data.Text             as Text
import qualified Data.Text.Encoding    as Text
import qualified MIO.Log               as Log
import qualified System.Exit           as System
import qualified System.Process.Typed  as Process

type Proc = Process.ProcessConfig () () ()

runProcess
  :: Env env
  => Process.ProcessConfig stdin stdout stderr
  -> MIO env System.ExitCode
runProcess proc = procRun proc Process.runProcess

runProcess_
  :: Env env
  => Process.ProcessConfig stdin stdout stderr
  -> MIO env ()
runProcess_ proc = procRun proc Process.runProcess_

readProcessStdout_
  :: Env env
  => Process.ProcessConfig stdin stdout stderr
  -> MIO env LBS.ByteString
readProcessStdout_ proc = procRun proc Process.readProcessStdout_

captureText
  :: Env env
  => Process.ProcessConfig stdin stdout stderr
  -> MIO env Text
captureText proc
  =   Text.strip
  .   Text.decodeUtf8
  .   LBS.toStrict
  <$> readProcessStdout_ proc

readProcessStdout
  :: Env env
  => Process.ProcessConfig stdin stdout stderr
  -> MIO env (System.ExitCode, LBS.ByteString)
readProcessStdout proc = procRun proc Process.readProcessStdout

procRun
  :: Env env
  => Process.ProcessConfig stdin stdout stderr
  -> (Process.ProcessConfig stdin stdout stderr -> IO a)
  -> MIO env a
procRun proc action = onDebug (Log.debug . convert $ show proc) >> liftIO (action proc)

silenceStderr :: Proc -> Proc
silenceStderr = Process.setStderr Process.nullStream

silenceStdout :: Proc -> Proc
silenceStdout = Process.setStdout Process.nullStream

silence :: Proc -> Proc
silence = silenceStdout . silenceStderr

exitBool :: System.ExitCode -> Bool
exitBool = (== System.ExitSuccess)

backendProc :: Env env => [String] -> MIO env Proc
backendProc arguments = do
  backend <- askBackend

  pure $ Process.proc (binaryName backend) arguments
