module LHT.RDS (setupRootCertificate) where

import LHT.Prelude
import LHT.TH

import qualified Data.Foldable         as Foldable
import qualified Data.Text.IO          as Text
import qualified System.Environment    as Environment
import qualified System.Path           as Path
import qualified System.Path.Directory as Path

setupRootCertificate :: IO ()
setupRootCertificate =
  Foldable.traverse_ setupEnv =<< Environment.lookupEnv "PGSSLROOTCERT"

setupEnv :: String -> IO ()
setupEnv value = maybe failParse createIfMissing $ Path.maybe value
  where
    failParse :: IO ()
    failParse
      = fail
      $ "Could not parse PGSSLROOTCERT value as absolute path: " <> show value

createIfMissing :: Path.AbsFile -> IO ()
createIfMissing rootCertFile = do
  Path.createDirectoryIfMissing True $ Path.takeDirectory rootCertFile
  exists <- Path.doesFileExist rootCertFile

  if exists
    then logRootCert "does already exist, skipping creation"
    else createFile

  where
    filepath = Path.toString rootCertFile

    createFile :: IO ()
    createFile = do
      logRootCert "does not exist, creating"
      Text.writeFile filepath rootCertificate

    logRootCert :: MonadIO m => String -> m ()
    logRootCert message
      = log
      $ "RDS root cert file: " <> filepath <> ", " <> message


#ifndef __HLINT__
rootCertificate :: Text
rootCertificate = $$(readFile $ Path.file "aws/rds-ca-2019-root.pem")
#endif
