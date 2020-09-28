module Main
  ( main
  )
where

import           Control.Monad.Reader           ( asks )
import           Data.Text                      ( Text )
import           MRIO.Colog
import           MRIO.Colog.Ref
import           Prelude

import qualified Devtools

data Env = Env
  { logAction :: LogAction (RIO Env) Message
  , appName   :: Text
  }

type instance LogActionField Env = "logAction"

data EnvWithRef = EnvWithRef
  { ref       :: MsgRef Message
  , refLogger :: LogAction (RIO EnvWithRef) Message
  , envName   :: Text
  }

type instance LogActionField EnvWithRef = "refLogger"
type instance RefField EnvWithRef = "ref"

main :: IO ()
main = do
  env' <- envWithRef

  runRioWithLogRef env' $ do
    name <- asks envName
    logInfo $ "App Name is " <> name

  runRIO env $ do
    name <- asks appName
    logInfo $ "App Name is " <> name

  Devtools.main Devtools.defaultConfig
 where
  env :: Env
  env = Env { logAction = richMessageAction, appName = "Test App" }

  envWithRef :: IO EnvWithRef
  envWithRef = do
    ref <- newMsgRef
    pure $ EnvWithRef { envName = "Ref App", refLogger = refLogAction, .. }
