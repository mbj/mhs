module CBT.Backend
  ( Backend(..)
  , binaryName
  , loadBackend
  )
where

import CBT.Prelude

import qualified System.Path.Directory as Path
import qualified UnliftIO.Environment  as Environment
import qualified UnliftIO.Exception    as Exception

data Backend = Docker | Podman

binaryName :: Backend -> String
binaryName = \case
  Docker -> "docker"
  Podman -> "podman"

loadBackend :: forall m . MonadUnliftIO m => m Backend
loadBackend =
  maybe auto explicit =<< Environment.lookupEnv "CBT_BACKEND"
  where
    explicit name = case name of
      "docker" -> loadBackend' Docker
      "podman" -> loadBackend' Podman
      other    -> Exception.throwString $ "Unknown CBT backend: " <> show other

    auto = selectBackend [Podman, Docker]

    selectBackend :: [Backend] -> m Backend
    selectBackend = \case
      []   -> Exception.throwString "No valid CBT backend found in PATH"
      x:xs -> maybe (selectBackend xs) pure =<< tryBackend x

    loadBackend' :: Backend -> m Backend
    loadBackend' backend
      = maybe
          (Exception.throwString $ "Requested backend not found in PATH: " <> binaryName backend)
          pure
      =<< tryBackend backend

    tryBackend :: Backend -> m (Maybe Backend)
    tryBackend backend =
      fmap (const backend) <$> liftIO (Path.findExecutable $ binaryName backend)
