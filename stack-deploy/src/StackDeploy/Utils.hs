module StackDeploy.Utils where

import StackDeploy.Prelude
import Stratosphere
import Stratosphere.Helpers

import qualified Amazonka.CloudFormation.Types as CF
import qualified Data.Aeson                    as JSON
import qualified Data.Foldable                 as Foldable
import qualified Data.Vector                   as Vector

mkName :: Val Text -> Val Text
mkName name = Join "-" [awsStackName, name]

awsAccountId :: Val Text
awsAccountId = toRef AccountId

awsRegion :: Val Text
awsRegion = toRef Region

awsStackName :: Val Text
awsStackName = toRef StackName

getAtt :: Text -> Resource -> Val Text
getAtt name item = GetAtt (itemName item) name

getAttArn :: Resource -> Val Text
getAttArn = getAtt "Arn"

s3BucketBlockPublicAccess :: S3BucketPublicAccessBlockConfiguration
s3BucketBlockPublicAccess
  = s3BucketPublicAccessBlockConfiguration
  & sbpabcBlockPublicAcls       ?~ Literal True
  & sbpabcBlockPublicPolicy     ?~ Literal True
  & sbpabcIgnorePublicAcls      ?~ Literal True
  & sbpabcRestrictPublicBuckets ?~ Literal True

allowResourcePolicy
  :: JSON.ToJSON b
  => (JSON.Object -> Val Text -> a)
  -> b
  -> Text
  -> [Text]
  -> a
allowResourcePolicy constructor resources name actions
  = constructor [("Statement", statement)] (Literal name)
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

dependencies :: [Resource] -> Resource -> Resource
dependencies deps res =
  res & resourceDependsOn ?~ (itemName <$> deps)

fetchOutput
  :: forall m . MonadFail m
  => CF.Stack
  -> Stratosphere.Output
  -> m Text
fetchOutput stack soutput =
  maybe
    (failOutputKey "missing")
    (maybe (failOutputKey "has no value") pure . getField @"outputValue")
    $ Foldable.find
      ((==) (pure key) . getField @"outputKey")
      (fromMaybe [] (getField @"outputs" stack))
  where
    key = getField @"_outputName" soutput

    failOutputKey :: Text -> m a
    failOutputKey message
      = failStack
      $ "Output " <> convertText key <> " " <> message

    failStack :: Text -> m a
    failStack message
      = fail
      . convertText
      $ "Stack: " <> getField @"stackName" stack <> " " <> message

resolveSecretsmanagerSecret :: Val Text -> Val Text
resolveSecretsmanagerSecret arn = wrap $ Join ":" ["resolve", "secretsmanager", arn]
  where
    wrap :: Val Text -> Val Text
    wrap value = Join "" ["{{", value, "}}"]

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

instance ToRef (PseudoParameter a) Text where
  toRef = Ref . convert . (<>) "AWS::" . show
