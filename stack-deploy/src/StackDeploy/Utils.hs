module StackDeploy.Utils where

import StackDeploy.Prelude

import qualified Amazonka.CloudFormation.Types as CF
import qualified Data.Aeson                    as JSON
import qualified Data.Foldable                 as Foldable
import qualified Data.Vector                   as Vector
import qualified Stratosphere                  as CFT
import qualified Stratosphere.S3.Bucket        as S3.Bucket

mkName :: CFT.Value Text -> CFT.Value Text
mkName name = CFT.Join "-" [awsStackName, name]

awsAccountId :: CFT.Value Text
awsAccountId = CFT.toRef AccountId

awsRegion :: CFT.Value Text
awsRegion = CFT.toRef Region

awsStackName :: CFT.Value Text
awsStackName = CFT.toRef StackName

getAtt :: Text -> CFT.Resource -> CFT.Value Text
getAtt name item = CFT.GetAtt (CFT.itemName item) name

getAttArn :: CFT.Resource -> CFT.Value Text
getAttArn = getAtt "Arn"

s3BucketBlockPublicAccess :: S3.Bucket.PublicAccessBlockConfigurationProperty
s3BucketBlockPublicAccess
  = S3.Bucket.PublicAccessBlockConfigurationProperty
  { S3.Bucket.blockPublicAcls       = pure $ CFT.Literal True
  , S3.Bucket.blockPublicPolicy     = pure $ CFT.Literal True
  , S3.Bucket.ignorePublicAcls      = pure $ CFT.Literal True
  , S3.Bucket.restrictPublicBuckets = pure $ CFT.Literal True
  }

allowResourcePolicy
  :: JSON.ToJSON b
  => (JSON.Object -> CFT.Value Text -> a)
  -> b
  -> Text
  -> [Text]
  -> a
allowResourcePolicy constructor resources name actions
  = constructor [("Statement", statement)] (CFT.Literal name)
  where
    statement :: JSON.Value
    statement = JSON.object
      [ ("Action",   JSON.Array $ Vector.fromList (JSON.toJSON <$> actions))
      , ("Effect",   "Allow")
      , ("Resource", JSON.toJSON resources)
      ]

assumeRole :: Text -> JSON.Object
assumeRole service =
  [ ("Version", JSON.String "2012-10-17")
  , ("Statement"
    , JSON.object
      [ ("Action",   "sts:AssumeRole")
      , ("Effect",   "Allow")
      , ("Principal", JSON.object [("Service", JSON.toJSON service)])
      ]
    )
  ]

dependencies :: [CFT.Resource] -> CFT.Resource -> CFT.Resource
dependencies deps resource =
  resource { CFT.dependsOn = pure (CFT.itemName <$> deps) }

fetchOutput
  :: forall m . MonadFail m
  => CF.Stack
  -> CFT.Output
  -> m Text
fetchOutput stack stratosphereOutput =
  maybe
    (failOutputKey "missing")
    (maybe (failOutputKey "has no value") pure . getField @"outputValue")
    $ Foldable.find
      ((==) (pure key) . (.outputKey))
      (fromMaybe [] stack.outputs)
  where
    key :: Text
    key = stratosphereOutput.name

    failOutputKey :: Text -> m a
    failOutputKey message
      = failStack
      $ "Output " <> convertText key <> " " <> message

    failStack :: Text -> m a
    failStack message
      = fail
      . convertText
      $ "Stack: " <> stack.stackName <> " " <> message

resolveSecretsmanagerSecret :: CFT.Value Text -> CFT.Value Text
resolveSecretsmanagerSecret arn = wrap $ CFT.Join ":" ["resolve", "secretsmanager", arn]
  where
    wrap :: CFT.Value Text -> CFT.Value Text
    wrap value = CFT.Join "" ["{{", value, "}}"]

-- See https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/pseudo-parameter-reference.html
data PseudoParameter a
  = AccountId
  | NoValue
  | NotificationARNs
  | Partition
  | Region
  | StackId
  | StackName
  | URLSuffix
  deriving stock Show

instance CFT.ToRef (PseudoParameter a) Text where
  toRef = CFT.Ref . convert . (<>) "AWS::" . show
