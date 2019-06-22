module OpenApi
  ( module OpenApi.Types
  , loadSpecFile
  )
where

import Data.Aeson
import Data.ByteString.Lazy (ByteString, readFile)
import OpenApi.Prelude
import OpenApi.Types
import System.FilePath (FilePath)

loadSpecFile
  :: forall m .(MonadFail m, MonadIO m)
  => FilePath
  -> m Specification
loadSpecFile = loadSpec <=< (liftIO . readFile)
  where
    loadSpec :: ByteString -> m Specification
    loadSpec
      = either (fail . ("Specification JSON decode failed: " <>)) pure
      . eitherDecode'
