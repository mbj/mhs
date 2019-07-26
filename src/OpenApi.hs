module OpenApi
  ( module OpenApi.Description
  , module OpenApi.Types
  , loadSpecFile
  )
where

import Data.ByteString.Lazy (ByteString)
import OpenApi.Description
import OpenApi.Prelude
import OpenApi.Types
import System.FilePath (FilePath)

import qualified Data.Aeson           as JSON
import qualified Data.ByteString.Lazy as ByteString

loadSpecFile
  :: forall m .(MonadFail m, MonadIO m)
  => FilePath
  -> m Specification
loadSpecFile = loadSpec <=< (liftIO . ByteString.readFile)
  where
    loadSpec :: ByteString -> m Specification
    loadSpec
      = either (fail . ("Specification JSON decode failed: " <>)) pure
      . JSON.eitherDecode'
