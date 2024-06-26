module StackDeploy.Stratosphere
  ( allowResourcePolicy
  , assumeRole
  , assumeRolePolicyDocument
  , dependencies
  , getAtt
  , getAttArn
  , mkLambdaLogGroup
  , mkLambdaLogsPolicy
  , mkName
  , mkResourceListOutputExport
  , mkResourceOutputExport
  , mkStackImport
  , mkStackListImport
  , resolveSecretsmanagerSecret
  , s3BucketBlockPublicAccess
  )
where

import StackDeploy.Prelude

import qualified Data.Aeson                 as JSON
import qualified Data.Vector                as Vector
import qualified Stratosphere               as CFT
import qualified Stratosphere.IAM.Role      as IAM
import qualified Stratosphere.Logs.LogGroup as Logs
import qualified Stratosphere.S3.Bucket     as S3.Bucket

mkName :: CFT.Value Text -> CFT.Value Text
mkName name = CFT.Join "-" [CFT.awsStackName, name]

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

assumeRolePolicyDocument :: CFT.Value Text -> JSON.Object
assumeRolePolicyDocument service =
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

resolveSecretsmanagerSecret :: CFT.Value Text -> CFT.Value Text
resolveSecretsmanagerSecret arn = wrap $ CFT.Join ":" ["resolve", "secretsmanager", arn]
  where
    wrap :: CFT.Value Text -> CFT.Value Text
    wrap value = CFT.Join "" ["{{", value, "}}"]

mkLambdaLogGroup :: CFT.Value Text -> Logs.LogGroup
mkLambdaLogGroup lambdaFunctionName
  = Logs.mkLogGroup
  & CFT.set @"LogGroupName" (CFT.Join "" ["/aws/lambda/", lambdaFunctionName])

mkLambdaLogsPolicy :: CFT.Value Text -> IAM.PolicyProperty
mkLambdaLogsPolicy lambdaFunctionName = IAM.mkPolicyProperty [("Statement", policyStatement)] "lambda-logging"
  where
    policyStatement :: JSON.Value
    policyStatement = JSON.Object
      [ ("Action",   JSON.Array ["logs:CreateLogStream", "logs:PutLogEvents"])
      , ("Effect",   JSON.String "Allow")
      , ("Resource", JSON.toJSON lambdaLogGroupArnValue)
      ]

    lambdaLogGroupArnValue :: CFT.Value Text
    lambdaLogGroupArnValue =
      CFT.Join
        ":"
        [ "arn"
        , "aws"
        , "logs"
        , CFT.awsRegion
        , CFT.awsAccountId
        , "log-group"
        , CFT.Join "/" ["/aws/lambda", lambdaFunctionName]
        , "*"
        ]

mkResourceListOutputExport :: CFT.Resources -> Text -> (CFT.Resource -> CFT.Value Text) -> CFT.Output
mkResourceListOutputExport resources outputName mkValue
  = mkOutputExport outputName (CFT.Join listDelimiter . CFT.ValueList $ mkValue <$> resources.resourceList)

mkResourceOutputExport :: CFT.Resource -> Text -> (CFT.Resource -> CFT.Value Text) -> CFT.Output
mkResourceOutputExport resource' attribute mkValue
  = mkOutputExport outputName (mkValue resource')
  where
    outputName :: Text
    outputName = resource'.logicalName <> attribute

mkOutputExport :: Text -> CFT.Value Text -> CFT.Output
mkOutputExport outputName outputValue
  = CFT.mkOutput outputName outputValue
  & CFT.set @"Export" (CFT.OutputExport $ mkOutputExportName outputName)

mkStackImport :: CFT.Value Text -> CFT.Output -> CFT.Value Text
mkStackImport stackName output = CFT.ImportValue $ mkStackOutputExportName stackName output.name

mkStackListImport :: CFT.Value Text -> CFT.Output -> CFT.ValueList Text
mkStackListImport stackName = CFT.Split listDelimiter . mkStackImport stackName

mkStackOutputExportName :: CFT.Value Text -> Text -> CFT.Value Text
mkStackOutputExportName stackName name = CFT.Join ":" [stackName, CFT.Literal name]

mkOutputExportName :: Text -> CFT.Value Text
mkOutputExportName = mkStackOutputExportName CFT.awsStackName

listDelimiter :: Text
listDelimiter = ","
