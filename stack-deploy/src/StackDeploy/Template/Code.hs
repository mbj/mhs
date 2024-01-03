module StackDeploy.Template.Code
  ( bucketNameOutput
  , namedTemplate
  )
where

import StackDeploy.NamedTemplate
import StackDeploy.Prelude
import StackDeploy.Stratosphere

import qualified Stratosphere           as CFT
import qualified Stratosphere.S3.Bucket as S3

namedTemplate :: NamedTemplate
namedTemplate
  = mkNamedTemplate (fromType @"code")
  $ CFT.mkTemplate [codeBucket]
  & CFT.set @"Outputs" [bucketNameOutput]

codeBucket :: CFT.Resource
codeBucket
  = CFT.resource "CodeBucket"
  $ S3.mkBucket
  & CFT.set @"PublicAccessBlockConfiguration" s3BucketBlockPublicAccess

bucketNameOutput :: CFT.Output
bucketNameOutput
  = CFT.mkOutput "CodeBucketName" (CFT.toRef codeBucket)
  & CFT.set @"Export" (CFT.OutputExport "CodeBucketName")
