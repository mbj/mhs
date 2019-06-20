module StackDeploy.Template.Code (template) where

import StackDeploy.Prelude
import StackDeploy.Utils
import Stratosphere hiding (template)

import qualified Stratosphere

template :: Template
template
  = Stratosphere.template [codeBucket]
  & templateOutputs ?~ outputs
  where
    outputs = Outputs
      [ output "CodeBucketName"
         (toRef codeBucket)
         & outputExport ?~ OutputExport "CodeBucketName"
      ]

codeBucket :: Resource
codeBucket
  = resource "CodeBucket"
  $ s3Bucket
  & sbPublicAccessBlockConfiguration ?~ s3BucketBlockPublicAccess
