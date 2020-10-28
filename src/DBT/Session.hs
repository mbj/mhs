module DBT.Session (Session(..)) where

import Control.Monad.Reader (asks)
import DBT.Connection
import DBT.Prelude

import qualified Hasql.Session      as Hasql
import qualified UnliftIO.Exception as Exception

class Session env where
  runSession       :: Hasql.Session a -> RIO env a
  runSessionEither :: Hasql.Session a -> RIO env (Either Hasql.QueryError a)

instance Session (ConnectionEnv env) where
  runSession               = either Exception.throwIO pure <=< runSessionEither
  runSessionEither session = liftIO . Hasql.run session =<< asks connection
