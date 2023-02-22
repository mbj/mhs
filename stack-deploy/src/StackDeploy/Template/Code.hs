module StackDeploy.Template.Code (template) where

import StackDeploy.Prelude
import StackDeploy.Template
import StackDeploy.Utils
import Stratosphere hiding (Template)

import qualified StackDeploy.Template   as Template
import qualified Stratosphere.S3.Bucket as S3

template :: Template
template
  = mk (Template.mkName "code")
  $ Stratosphere.mkTemplate [codeBucket]
  & set @"Outputs" outputs
  where
    outputs
      = Outputs
      [ mkOutput "CodeBucketName" (toRef codeBucket)
      & set @"Export" (OutputExport "CodeBucketName")
      ]

codeBucket :: Resource
codeBucket
  = resource "CodeBucket"
  $ S3.mkBucket
  & set @"PublicAccessBlockConfiguration" s3BucketBlockPublicAccess
