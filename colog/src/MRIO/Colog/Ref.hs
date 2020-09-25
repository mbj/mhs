-- | This module provides a way of storing and accumulating logs via an IORef
-- so as to print them in one call during a RIO application evaluation.
--
{-# OPTIONS -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module MRIO.Colog.Ref
  ( module Colog.Prelude
  , LogActionField
  , MsgRef(..)
  , RefField
  , newMsgRef
  , refLogAction
  , runRioWithLogRef
  )
where

import           Control.Monad.Reader
import           Data.Foldable
import           Data.Functor                   ( ($>) )
import           Data.IORef
import           Data.Kind                      ( Type )
import           Data.Sequence
import           GHC.Records                    ( HasField
                                                , getField
                                                )
import           GHC.TypeLits                   ( Symbol )
import           MRIO.Colog
import           MRIO.Colog.Prelude            as Colog.Prelude
import           Prelude

import qualified Data.Text.IO                  as Text
import qualified System.IO                     as IO

newtype MsgRef msg = MsgRef (IORef (Seq msg))

type family RefField (env :: Type) :: Symbol

refLogAction
  :: forall field msg env
   . (field ~ RefField env, HasField field env (MsgRef msg))
  => LogAction (RIO env) msg
refLogAction = LogAction $ \message -> do
  MsgRef ioRef <- asks $ getField @field

  liftIO $ modifyIORef' ioRef (|> message)

newMsgRef :: MonadIO m => m (MsgRef msg)
newMsgRef = MsgRef <$> liftIO (newIORef empty)

runRioWithLogRef
  :: forall field env a
   . (field ~ RefField env, HasField field env (MsgRef Message))
  => env
  -> RIO env a
  -> IO a
runRioWithLogRef env rio = runRIO env $ do
  result <- rio
  runMessages $> result
 where
  runMessages :: RIO env ()
  runMessages = do
    MsgRef ioRef <- asks $ getField @field

    messages     <- liftIO $ readIORef ioRef
    liftIO $ traverse_ logAsRichMessage messages

logAsRichMessage :: Message -> IO ()
logAsRichMessage message = do
  richMessage <- fmtRichMessageDefault richMessageIO
  Text.hPutStrLn IO.stderr richMessage
 where
  richMessageIO :: RichMessage IO
  richMessageIO =
    RichMsg { richMsgMsg = message, richMsgMap = defaultFieldMap }
