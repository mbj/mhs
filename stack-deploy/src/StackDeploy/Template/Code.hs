module StackDeploy.Template.Code (template) where

import StackDeploy.Prelude
import StackDeploy.Template
import StackDeploy.Utils
import Stratosphere hiding (Template, template)

import qualified StackDeploy.Template as Template
import qualified Stratosphere

template :: Template
template
  = mk (Template.mkName "code")
  $ Stratosphere.template [codeBucket]
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
