module StackDeploy.ParameterDeploy
  ( ParameterDeployConfig(..)
  , parameterDeployComponent
  , parameterDeployLambdaFunctionObjectKey
  , parameterDeployLambdaInvoke
  , parameterDeployLambdaInvokeFunctionPolicy
  , parameterDeployLambdaMain
  , parameterDeployParserInfo
  )
where

import CLI.Utils
import Control.Lens ((?~))
import StackDeploy.Prelude
import StackDeploy.Types

import qualified AWS.Lambda.Runtime                    as Lambda.Runtime
import qualified Amazonka.CloudFormation.Types         as CloudFormation
import qualified Amazonka.CloudFormation.UpdateStack   as CloudFormation
import qualified Amazonka.Lambda.Invoke                as Lambda
import qualified Data.Aeson                            as JSON
import qualified Data.Aeson.Types                      as JSON
import qualified Data.List                             as List
import qualified Data.Map.Strict                       as Map
import qualified Data.Set                              as Set
import qualified MIO.Amazonka                          as AWS
import qualified Options.Applicative                   as CLI
import qualified StackDeploy.CLI.Utils                 as StackDeploy
import qualified StackDeploy.Component                 as StackDeploy
import qualified StackDeploy.EnvSpec                   as StackDeploy
import qualified StackDeploy.IO                        as StackDeploy
import qualified StackDeploy.InstanceSpec              as StackDeploy
import qualified StackDeploy.Operation                 as StackDeploy
import qualified StackDeploy.Parameters                as StackDeploy
import qualified StackDeploy.Stack                     as StackDeploy
import qualified StackDeploy.Stratosphere              as CFT
import qualified StackDeploy.Types                     as StackDeploy
import qualified StackDeploy.Wait                      as StackDeploy
import qualified Stratosphere                          as CFT
import qualified Stratosphere.CloudWatch.Alarm         as CloudWatch
import qualified Stratosphere.IAM.Role                 as IAM
import qualified Stratosphere.Lambda.EventInvokeConfig as Lambda
import qualified Stratosphere.Lambda.Function          as Lambda
import qualified System.Exit                           as System

data ParameterDeployConfig = ParameterDeployConfig
  { alarmTopic            :: CFT.Resource
  , allowedParameterNames :: Set StackDeploy.ParameterName
  , capabilities          :: [CloudFormation.Capability]
  , functionName          :: Text
  , s3BucketName          :: CFT.Value Text
  }

newtype ParameterDeployLambdaResponse = ParameterDeployLambdaResponse
  { token :: Maybe StackDeploy.Token
  }
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)
  deriving stock (Generic)

parameterDeploy
  :: forall env . AWS.Env env
  => ParameterDeployConfig
  -> StackDeploy.InstanceName
  -> StackDeploy.ParameterMap
  -> MIO env (Maybe StackDeploy.Token)
parameterDeploy ParameterDeployConfig{..} stackName userParameters = do
  existingStack   <- StackDeploy.readExistingStackPresent stackName
  inputParameters <- parseInputParameterMap
  token           <- StackDeploy.newToken

  newParameters <- either throwString pure $
    StackDeploy.parameterMapExpand
      (StackDeploy.ParameterExpandUpdate existingStack.parameterNames)
      existingStack.parameterNames
      inputParameters

  either
    (StackDeploy.allowNoUpdateError empty)
    (const . pure $ pure token) =<< AWS.sendEither (updateRequest existingStack.stackId newParameters token)
  where
    parseInputParameterMap =
      if Set.null unexpected
        then pure userParameters
        else throwString $
          "Parameters not allowed for update: " <> List.intercalate "," (convertVia @Text <$> Set.toList unexpected)
      where
        unexpected = Set.difference (Map.keysSet userParameters) allowedParameterNames

    updateRequest :: StackDeploy.StackId -> [CloudFormation.Parameter] -> StackDeploy.Token -> CloudFormation.UpdateStack
    updateRequest stackId newParameters token
      = CloudFormation.newUpdateStack (convert stackId)
      & CloudFormation.updateStack_capabilities        ?~ capabilities
      & CloudFormation.updateStack_parameters          ?~ newParameters
      & CloudFormation.updateStack_usePreviousTemplate ?~ True
      & CloudFormation.updateStack_clientRequestToken  ?~ convert token

parameterDeployLambdaFunctionObjectKey :: CFT.Parameter
parameterDeployLambdaFunctionObjectKey
  = CFT.mkParameter "ParameterDeployLambdaFunctionObjectKey" "String"
  & CFT.set @"Description" "S3 ObjectKey in for the parameter deployment lambda"

parameterDeployLambdaMain :: AWS.Env env => ParameterDeployConfig -> MIO env ()
parameterDeployLambdaMain config = do
  instanceName <- convertThrow =<< StackDeploy.readEnvSpecFromEnvironment stackNameEnvSpec

  Lambda.Runtime.run $ \Lambda.Runtime.Event{..} -> do
    token <- parameterDeploy config instanceName =<< either throwString pure (JSON.parseEither JSON.parseJSON body)
    pure . JSON.toJSON $ ParameterDeployLambdaResponse{..}

parameterDeployLambdaInvokeFunctionPolicy :: ParameterDeployConfig -> IAM.PolicyProperty
parameterDeployLambdaInvokeFunctionPolicy config =
  CFT.allowResourcePolicy
    IAM.mkPolicyProperty
    (CFT.getAttArn $ function config)
    "parameter-deploy-invoke"
    ["lambda:InvokeFunction"]

stackNameEnvSpec :: StackDeploy.EnvSpec
stackNameEnvSpec = StackDeploy.EnvSpec (fromType @"STACK_NAME") StackDeploy.EnvSpecStackName

parameterDeployComponent :: ParameterDeployConfig -> StackDeploy.Component
parameterDeployComponent config@ParameterDeployConfig{..} = mempty
  { StackDeploy.outputs    = [functionNameOutput config]
  , StackDeploy.parameters = [parameterDeployLambdaFunctionObjectKey]
  , StackDeploy.resources  =
      [ alarmErrors
      , eventInvokeConfig
      , function config
      , logGroup
      , role config & CFT.set @"DependsOn" [CFT.itemName logGroup]
      ]
  }
  where
    mkLogicalName :: Text -> Text
    mkLogicalName = (functionName <>)

    logGroup :: CFT.Resource
    logGroup
      = CFT.resource (mkLogicalName "LogGroup")
      $ CFT.mkLambdaLogGroup (lambdaFunctionName config)

    eventInvokeConfig :: CFT.Resource
    eventInvokeConfig
      = CFT.resource (mkLogicalName "EventInvokeConfig")
      $ Lambda.mkEventInvokeConfig (CFT.toRef $ function config) "$LATEST"
      & CFT.set @"MaximumEventAgeInSeconds" (CFT.Literal 60)
      & CFT.set @"MaximumRetryAttempts"     (CFT.Literal 0)

    alarmErrors :: CFT.Resource
    alarmErrors
      = CFT.resource (mkLogicalName "AlarmErrors")
      $ CloudWatch.mkAlarm "GreaterThanThreshold" (CFT.Literal 1)
      & CFT.set @"AlarmActions"      [CFT.toRef alarmTopic]
      & CFT.set @"DatapointsToAlarm" (CFT.Literal 1)
      & CFT.set @"Dimensions"        [CloudWatch.mkDimensionProperty "FunctionName" . CFT.toRef $ function config]
      & CFT.set @"MetricName"        "Errors"
      & CFT.set @"Namespace"         "AWS/Lambda"
      & CFT.set @"OKActions"         [CFT.toRef alarmTopic]
      & CFT.set @"Period"            (CFT.Literal 60)
      & CFT.set @"Statistic"         "Maximum"
      & CFT.set @"Threshold"         (CFT.Literal 0)
      & CFT.set @"TreatMissingData"  "notBreaching"

functionNameOutput :: ParameterDeployConfig -> CFT.Output
functionNameOutput config@ParameterDeployConfig{..}
  = CFT.mkOutput (functionName <> "LambdaFunctionName")
  . CFT.toRef
  $ function config

lambdaFunctionName :: ParameterDeployConfig -> CFT.Value Text
lambdaFunctionName ParameterDeployConfig{..} = CFT.mkName $ CFT.Literal functionName

role :: ParameterDeployConfig -> CFT.Resource
role config@ParameterDeployConfig{..}
  = CFT.resource (functionName <> "LambdaRole")
  $ IAM.mkRole (CFT.assumeRole "lambda.amazonaws.com")
  & CFT.set @"Policies" policies
  where
    iamPolicies :: [IAM.PolicyProperty]
    iamPolicies =
      [ CFT.allowResourcePolicy
        IAM.mkPolicyProperty
        CFT.awsStackId
        "cloudformation-update-stack"
        [ "cloudformation:DescribeStacks"
        , "cloudformation:UpdateStack"
        ]
      ]

    policies :: [IAM.PolicyProperty]
    policies
      =  [allowLogs, allowVPCNetworkInterface]
      <> iamPolicies

    allowLogs :: IAM.PolicyProperty
    allowLogs = CFT.mkLambdaLogsPolicy (lambdaFunctionName config)

    allowVPCNetworkInterface :: IAM.PolicyProperty
    allowVPCNetworkInterface = CFT.allowResourcePolicy
      IAM.mkPolicyProperty
      -- TODO Make sure to constrain this again
      -- Could be an AWS bug.
      -- Still better than the default role.
      ("*" :: CFT.Value Text) -- (mkSubnetArn <$> unResources Network.lambdaSubnets)
      "allow-vpc-network-interface"
      [ "ec2:CreateNetworkInterface"
      , "ec2:DeleteNetworkInterface"
      , "ec2:DescribeNetworkInterfaces"
      ]

function :: ParameterDeployConfig -> CFT.Resource
function config@ParameterDeployConfig{..}
  = CFT.resource (functionName <> "LambdaFunction")
  $ Lambda.mkFunction code (CFT.getAttArn $ role config)
  & CFT.set @"Environment"  environment
  & CFT.set @"FunctionName" (lambdaFunctionName config)
  & CFT.set @"Handler"      (CFT.Literal functionName)
  & CFT.set @"MemorySize"   (CFT.Literal 256)
  & CFT.set @"Runtime"      "provided.al2"
  & CFT.set @"Timeout"      (CFT.Literal 10)
  where
    environment :: Lambda.EnvironmentProperty
    environment = StackDeploy.envSpecLambdaEnvironment [stackNameEnvSpec]

    code :: Lambda.CodeProperty
    code
      = Lambda.mkCodeProperty
      & CFT.set @"S3Bucket" s3BucketName
      & CFT.set @"S3Key"    (CFT.toRef parameterDeployLambdaFunctionObjectKey)

parameterDeployParserInfo
  :: forall env . AWS.Env env
  => ParameterDeployConfig
  -> CLI.ParserInfo (MIO env System.ExitCode)
parameterDeployParserInfo config = wrapHelper commands "parameter deploy commands"
  where
    commands :: CLI.Parser (MIO env System.ExitCode)
    commands = CLI.hsubparser $ mkCommand
      "deploy"
      (parameterDeployLambdaInvoke config <$> StackDeploy.instanceNameOption <*> parameterMapArguments)
      "trigger deployment"

    parameterMapArguments :: CLI.Parser StackDeploy.ParameterMap
    parameterMapArguments = pure []

parameterDeployLambdaInvoke
  :: forall env . AWS.Env env
  => ParameterDeployConfig
  -> StackDeploy.InstanceName
  -> StackDeploy.ParameterMap
  -> MIO env System.ExitCode
parameterDeployLambdaInvoke config instanceName parameterMap = do
  existingStack <- StackDeploy.readExistingStackPresent instanceName
  functionName  <- StackDeploy.fetchStackOutput existingStack.stack (functionNameOutput config)

  ParameterDeployLambdaResponse{..} <- AWS.send (request functionName)
    >>= checkError
    >>= maybe (throwString "Empty lambda Respons") pure . (.payload)
    >>= either throwString pure . JSON.eitherDecode . convert

  maybe
    (StackDeploy.say ("No stack updates are to be performed" :: Text) >> StackDeploy.success)
    (wait existingStack.stackId)
    token
  where
    wait
      :: StackDeploy.StackId
      -> StackDeploy.Token
      -> MIO env System.ExitCode
    wait stackId token =
      StackDeploy.exitCode =<< StackDeploy.waitForAccept RemoteOperation{..} StackDeploy.printEvent

    request functionName
      = Lambda.newInvoke functionName
      . convert
      $ JSON.encode parameterMap

    checkError :: Lambda.InvokeResponse -> MIO env Lambda.InvokeResponse
    checkError response
      = maybe
        (pure response)
        (const . throwString $ "Lambda execution failed: " <> show response)
        response.functionError
