{-# LANGUAGE TemplateHaskell #-}

module AWS.RDS
  ( certificateAuthorityContents
  , mkConnectPolicy
  , populateCertificateAuthorityFile
  )
where

import Data.Conversions
import Data.FileEmbed
import MIO.Core
import MPrelude
import System.Path ((</>))

import qualified Data.Aeson            as JSON
import qualified Data.ByteString       as BS
import qualified MIO.Log               as Log
import qualified Stratosphere          as CFT
import qualified Stratosphere.IAM.Role as IAM
import qualified System.Path           as Path
import qualified System.Path.Directory as Path

populateCertificateAuthorityFile :: Log.Env env => MIO env ()
populateCertificateAuthorityFile = do
  home <- liftIO Path.getHomeDirectory

  let caFileDir = home </> Path.dir ".postgresql"

  liftIO $ Path.createDirectoryIfMissing True caFileDir

  let caFileString = Path.toString $ caFileDir </> Path.file "root.crt"

  Log.info $ "Writing RDS CA to " <> convert caFileString

  liftIO $ BS.writeFile caFileString certificateAuthorityContents

certificateAuthorityContents :: BS.ByteString
certificateAuthorityContents = $(embedFile "global-bundle.pem")

mkConnectPolicy :: CFT.Value Text -> CFT.Value Text -> IAM.PolicyProperty
mkConnectPolicy dbResourceId dbUserName = IAM.mkPolicyProperty [("Statement", policyStatement)] "rds-connect"
  where
    policyStatement :: JSON.Value
    policyStatement = JSON.Object
      [ ("Action",   "rds-db:connect")
      , ("Effect",   "Allow")
      , ("Resource", JSON.toJSON arn)
      ]

    arn
      = CFT.Join ":"
      [ "arn"
      , "aws"
      , "rds-db"
      , CFT.awsRegion
      , CFT.awsAccountId
      , "dbuser"
      , CFT.Join "/" [dbResourceId, dbUserName]
      ]
