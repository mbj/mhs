{-# OPTIONS -fno-warn-orphans #-}
module MRIO.Colog
  ( module Colog.Prelude
  , LogActionField
  )
where

import           Data.Kind                      ( Type )
import           GHC.Records                    ( HasField
                                                , getField
                                                )
import           GHC.TypeLits                   ( Symbol )
import           MRIO.Colog.Prelude            as Colog.Prelude
import           Prelude

type family LogActionField (env :: Type) :: Symbol

instance (field ~ LogActionField env, HasField field env (LogAction (RIO env) msg))
  => HasLog env msg (RIO env) where

  getLogAction env = getField @field env

  setLogAction = overLogAction . const
