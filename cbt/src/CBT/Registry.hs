module CBT.Registry
  ( Password(..)
  , Username(..)
  , login
  )
where

import CBT.Config
import CBT.Prelude
import CBT.Proc

import qualified CBT.Image.Name       as CBT.Image
import qualified Data.Text.Encoding   as Text
import qualified System.Process.Typed as Process

newtype Username = Username Text
  deriving (Conversion Text) via Text

newtype Password = Password Text
  deriving (Conversion Text) via Text

login
  :: Env env
  => CBT.Image.Registry
  -> Username
  -> Password
  -> MIO env ()
login registry username password
  = runProcess_
  . Process.setStdin
    ( Process.byteStringInput
    . convert
    . Text.encodeUtf8
    $ toText password
    )
  =<< backendProc
  [ "login"
  , "--username"
  , convertText username
  , "--password-stdin"
  , convertText registry
  ]
