module StackDeploy.Operation
  ( performOperation
  , printEvent
  )
where

import Control.Lens (Lens', set, view)
import Data.Int (Int)
import Data.Time.Format (defaultTimeLocale, formatTime)
import StackDeploy.IO
import StackDeploy.InstanceSpec (InstanceSpec(..))
import StackDeploy.Parameters
import StackDeploy.Prelude
import StackDeploy.Types
import StackDeploy.Wait

import qualified Amazonka
import qualified Amazonka.CloudFormation.CreateStack    as CF
import qualified Amazonka.CloudFormation.DeleteStack    as CF
import qualified Amazonka.CloudFormation.Types          as CF
import qualified Amazonka.CloudFormation.UpdateStack    as CF
import qualified Amazonka.S3.Types                      as S3
import qualified Data.ByteString                        as BS
import qualified Data.ByteString.Lazy                   as LBS
import qualified Data.Map.Strict                        as Map
import qualified Data.Text                              as Text
import qualified Data.Text.Encoding                     as Text
import qualified Data.Text.IO                           as Text
import qualified MIO.Amazonka                           as AWS
import qualified StackDeploy.Env                        as StackDeploy
import qualified StackDeploy.NamedTemplate              as StackDeploy
import qualified StackDeploy.S3
import qualified StackDeploy.Stack                      as StackDeploy

data OperationFields a = OperationFields
  { capabilitiesField  :: Lens' a (Maybe [CF.Capability])
  , parameterExpand    :: ParameterExpand
  , parametersField    :: Lens' a (Maybe [CF.Parameter])
  , roleARNField       :: Lens' a (Maybe Text)
  , templateBodyField  :: Lens' a (Maybe Text)
  , templateURLField   :: Lens' a (Maybe Text)
  , tokenField         :: Lens' a (Maybe Text)
  }

performOperation
  :: forall env . (AWS.Env env, StackDeploy.Env env)
  => Operation env
  -> MIO env RemoteOperationResult
performOperation = \case
  (OpCreate instanceSpec userParameterMap) -> do
    effectiveInstanceSpec <- loadEffectiveInstanceSpec instanceSpec
    successCallback instanceSpec =<< create effectiveInstanceSpec userParameterMap
  (OpDelete existingStack) ->
    runStackId existingStack.stackId delete
  (OpUpdate existingStack instanceSpec userParameterMap) -> do
    effectiveInstanceSpec <- loadEffectiveInstanceSpec instanceSpec
    successCallback effectiveInstanceSpec =<<
      runStackId
        existingStack.stackId
        (update effectiveInstanceSpec existingStack.parameters userParameterMap)
  where
    loadEffectiveInstanceSpec :: InstanceSpec env -> MIO env (InstanceSpec env)
    loadEffectiveInstanceSpec instanceSpec = instanceSpec.onLoad instanceSpec

    runStackId
      :: StackId
      -> (RemoteOperation -> MIO env RemoteOperationResult)
      -> MIO env RemoteOperationResult
    runStackId stackId action = do
      token <- newToken
      action RemoteOperation{..}

    create
      :: InstanceSpec env
      -> ParameterMap
      -> MIO env RemoteOperationResult
    create instanceSpec@InstanceSpec{..} userParameterMap = do
      token   <- newToken
      stackId <- StackDeploy.readStackIdField CF.createStackResponse_stackId =<< doCreate token
      waitFor RemoteOperation{..}
      where
        doCreate :: Token -> MIO env (Amazonka.AWSResponse CF.CreateStack)
        doCreate token =
          prepareOperation
            operationFields
            instanceSpec
            userParameterMap
            token
            (CF.newCreateStack $ convert name)
          >>= AWS.send

        operationFields = OperationFields
          { capabilitiesField = CF.createStack_capabilities
          , parameterExpand   = ParameterExpandCreate
          , parametersField   = CF.createStack_parameters
          , roleARNField      = CF.createStack_roleARN
          , templateBodyField = CF.createStack_templateBody
          , templateURLField  = CF.createStack_templateURL
          , tokenField        = CF.createStack_clientRequestToken
          }

    delete :: RemoteOperation -> MIO env RemoteOperationResult
    delete remoteOperation@RemoteOperation{..} =
      doDelete >> waitFor remoteOperation
      where
        doDelete :: MIO env ()
        doDelete
          = void
          . AWS.send
          . setText CF.deleteStack_clientRequestToken token
          . CF.newDeleteStack
          $ convert stackId

    update
      :: InstanceSpec env
      -> [CF.Parameter]
      -> ParameterMap
      -> RemoteOperation
      -> MIO env RemoteOperationResult
    update instanceSpec previousParameters userParameterMap remoteOperation@RemoteOperation{..}
      = either handleNoUpdateError (const $ waitFor remoteOperation) =<< doUpdate
      where
        doUpdate :: MIO env (Either Amazonka.Error CF.UpdateStackResponse)
        doUpdate =
          prepareOperation
            operationFields
            instanceSpec
            userParameterMap
            token
            (CF.newUpdateStack $ convert stackId)
          >>= AWS.sendEither

        handleNoUpdateError :: Amazonka.Error -> MIO env RemoteOperationResult
        handleNoUpdateError = \case
          ( Amazonka.ServiceError
            Amazonka.ServiceError'
            { code    = Amazonka.ErrorCode "ValidationError"
            , message = Just (Amazonka.ErrorMessage "No updates are to be performed.")
            }) -> pure RemoteOperationSuccess
          other -> throwIO other

        operationFields = OperationFields
          { capabilitiesField  = CF.updateStack_capabilities
          , parameterExpand    = ParameterExpandUpdate previousParameters
          , parametersField    = CF.updateStack_parameters
          , roleARNField       = CF.updateStack_roleARN
          , templateBodyField  = CF.updateStack_templateBody
          , templateURLField   = CF.updateStack_templateURL
          , tokenField         = CF.updateStack_clientRequestToken
          }

    waitFor :: RemoteOperation -> MIO env RemoteOperationResult
    waitFor remoteOperation = waitForAccept remoteOperation printEvent

    successCallback
      :: InstanceSpec env
      -> RemoteOperationResult
      -> MIO env RemoteOperationResult
    successCallback InstanceSpec{..} result = case result of
      RemoteOperationSuccess -> onSuccess >> pure result
      _                      -> pure result

printEvent :: CF.StackEvent -> MIO env ()
printEvent event = do
  say $ Text.unwords
    [ timestamp
    , physicalResourceId
    , logicalResourceId
    , resourceType
    , resourceStatus
    ]
  sayReason event.resourceStatusReason
  where
    logicalResourceId =
      fromMaybe
        "[unknown-logical-resource-id]"
        (event.logicalResourceId)

    physicalResourceId =
      fromMaybe
        "[unknown-physical-resource-id]"
        (event.physicalResourceId)

    resourceType =
      fromMaybe
        "[unknown-resource-type]"
        event.resourceType

    resourceStatus :: Text
    resourceStatus =
      maybe
        "[unknown-resource-type]"
        CF.fromResourceStatus
        event.resourceStatus

    timeFormat :: String
    timeFormat = "%Y-%m-%dT%H:%M:%S"

    timestamp :: Text
    timestamp
      = convertText
      . formatTime defaultTimeLocale timeFormat
      $ view CF.stackEvent_timestamp event

    sayReason :: Maybe Text -> MIO env ()
    sayReason = maybe (pure ()) (say . ("- " <>))

prepareOperation
  :: forall env a . (AWS.Env env, StackDeploy.Env env)
  => OperationFields a
  -> InstanceSpec env
  -> ParameterMap
  -> Token
  -> a
  -> MIO env a
prepareOperation
  OperationFields{..}
  InstanceSpec{..}
  userParameterMap
  token
  operation = do
    effectiveParameters <- either (throwString . show) pure $
      parameterMapTemplateExpand parameterExpand mergedParameterMap namedTemplate.template
    setTemplateBody
      . set     capabilitiesField (pure capabilities)
      . set     parametersField   (pure effectiveParameters)
      . set     roleARNField      (convert <$> roleArn)
      . setText tokenField        token
      $ operation
  where
    mergedParameterMap = Map.union parameterMap userParameterMap

    setTemplateBody :: a -> MIO env a
    setTemplateBody request =
      if BS.length templateBodyBS <= maxBytes
        then pure $ setText templateBodyField templateBody request
        else s3Template request

    s3Template :: a -> MIO env a
    s3Template request = do
      ask >>=
        (maybe failMissingTemplateBucket (doUpload request =<<) . (.readTemplateBucketName)) . (.stackDeployConfig)

    doUpload :: a -> S3.BucketName -> MIO env a
    doUpload request bucketName@(S3.BucketName bucketNameText) = do
      StackDeploy.S3.syncTarget targetObject
      pure $ setText templateURLField s3URL request
      where
        s3URL = "https://" <> bucketNameText <> ".s3.amazonaws.com/" <> StackDeploy.S3.targetObjectKeyText targetObject

        targetObject =
          (StackDeploy.S3.hashedTargetObject bucketName (convert name) "json" templateBodyBS)
            { StackDeploy.S3.uploadCallback = liftIO . Text.putStrLn . ("Uploading template: " <>)
            }

    failMissingTemplateBucket :: MIO env a
    failMissingTemplateBucket
      = liftIO
      $ fail
      $ "Template is bigger than "
      <> show maxBytes
      <> " cloudformation requires to read the template via an S3 object but the environment specifies none"

    maxBytes :: Int
    maxBytes = 51200

    templateBody   = Text.decodeUtf8 templateBodyBS
    templateBodyBS = LBS.toStrict $ StackDeploy.stratosphereTemplateEncodePretty namedTemplate.template

setText :: (Applicative f, ToText b) => Lens' a (f Text) -> b -> a -> a
setText field value = set field (pure $ convert value)
