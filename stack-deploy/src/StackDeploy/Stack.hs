module StackDeploy.Stack
  ( finalMessage
  , getExistingStack
  , getExistingStackId
  , getOutput
  , getStackId
  , perform
  , printEvent
  , stackNames
  )
where

import Control.Lens (Lens', set, view)
import Data.Conduit (ConduitT, (.|), runConduit)
import Data.Conduit.Combinators (find, map)
import Data.Int (Int)
import Data.Time.Format (defaultTimeLocale, formatTime)
import StackDeploy.IO
import StackDeploy.InstanceSpec (InstanceSpec(..))
import StackDeploy.Prelude
import StackDeploy.Types
import StackDeploy.Wait

import qualified Amazonka
import qualified Amazonka.CloudFormation.CreateStack    as CF
import qualified Amazonka.CloudFormation.DeleteStack    as CF
import qualified Amazonka.CloudFormation.DescribeStacks as CF
import qualified Amazonka.CloudFormation.Types          as CF
import qualified Amazonka.CloudFormation.UpdateStack    as CF
import qualified Amazonka.S3.Types                      as S3
import qualified Data.ByteString                        as BS
import qualified Data.ByteString.Lazy                   as LBS
import qualified Data.Foldable                          as Foldable
import qualified Data.Text                              as Text
import qualified Data.Text.Encoding                     as Text
import qualified Data.Text.IO                           as Text
import qualified MIO.Amazonka                           as AWS
import qualified StackDeploy.AWS                        as AWS
import qualified StackDeploy.Env                        as StackDeploy
import qualified StackDeploy.InstanceSpec               as InstanceSpec
import qualified StackDeploy.Parameters                 as Parameters
import qualified StackDeploy.S3                         as S3
import qualified StackDeploy.Template                   as Template

data OperationFields a = OperationFields
  { tokenField        :: Lens' a (Maybe Text)
  , capabilitiesField :: Lens' a (Maybe [CF.Capability])
  , parametersField   :: Lens' a (Maybe [CF.Parameter])
  , roleARNField      :: Lens' a (Maybe Text)
  , templateBodyField :: Lens' a (Maybe Text)
  , templateURLField  :: Lens' a (Maybe Text)
  }

perform
  :: forall env . (AWS.Env env, StackDeploy.Env env)
  => Operation env
  -> MIO env RemoteOperationResult
perform = \case
  (OpCreate instanceSpec) ->
    successCallback instanceSpec =<< create instanceSpec
  (OpDelete stackId) ->
    runStackId stackId delete
  (OpUpdate stackId instanceSpec) ->
    successCallback instanceSpec =<<
      runStackId stackId (update instanceSpec)
  where
    runStackId
      :: Id
      -> (RemoteOperation -> MIO env RemoteOperationResult)
      -> MIO env RemoteOperationResult
    runStackId stackId action = do
      token <- newToken
      action RemoteOperation{..}

    create :: InstanceSpec env -> MIO env RemoteOperationResult
    create instanceSpec@InstanceSpec{..} = do
      token   <- newToken
      stackId <- accessStackId CF.createStackResponse_stackId =<< doCreate token
      waitFor RemoteOperation{..}
      where
        doCreate :: Token -> MIO env (Amazonka.AWSResponse CF.CreateStack)
        doCreate token =
          prepareOperation operationFields instanceSpec token (CF.newCreateStack $ toText name)
            >>= AWS.send

        operationFields = OperationFields
          { capabilitiesField = CF.createStack_capabilities
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
          . CF.newDeleteStack $ toText stackId

    update
      :: InstanceSpec env
      -> RemoteOperation
      -> MIO env RemoteOperationResult
    update
      instanceSpec
      remoteOperation@RemoteOperation{..} =
        catchJust handleNoUpdateError
          (doUpdate >> waitFor remoteOperation)
          pure
      where
        doUpdate :: MIO env ()
        doUpdate =
          prepareOperation operationFields instanceSpec token (CF.newUpdateStack $ toText stackId)
            >>= void . AWS.send

        handleNoUpdateError :: Amazonka.Error -> Maybe RemoteOperationResult
        handleNoUpdateError
          ( Amazonka.ServiceError
            Amazonka.ServiceError'
            { _serviceErrorCode = Amazonka.ErrorCode "ValidationError"
            , _serviceErrorMessage = Just (Amazonka.ErrorMessage "No updates are to be performed.")
            }
          ) = pure RemoteOperationSuccess
        handleNoUpdateError _error = empty

        operationFields = OperationFields
          { capabilitiesField = CF.updateStack_capabilities
          , parametersField   = CF.updateStack_parameters
          , roleARNField      = CF.updateStack_roleARN
          , templateBodyField = CF.updateStack_templateBody
          , templateURLField  = CF.updateStack_templateURL
          , tokenField        = CF.updateStack_clientRequestToken
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
  sayReason $ getField @"resourceStatusReason" event
  where
    logicalResourceId =
      fromMaybe
        "[unknown-logical-resource-id]"
        (getField @"logicalResourceId" event)

    physicalResourceId =
      fromMaybe
        "[unknown-physical-resource-id]"
        (getField @"physicalResourceId" event)

    resourceType =
      fromMaybe
        "[unknown-resource-type]"
        (getField @"resourceType" event)

    resourceStatus :: Text
    resourceStatus =
      maybe
        "[unknown-resource-type]"
        CF.fromResourceStatus
        (getField @"resourceStatus" event)

    timeFormat :: String
    timeFormat = "%Y-%m-%dT%H:%M:%S"

    timestamp :: Text
    timestamp
      = convertText
      . formatTime defaultTimeLocale timeFormat
      $ view CF.stackEvent_timestamp event

    sayReason :: Maybe Text -> MIO env ()
    sayReason = maybe (pure ()) (say . ("- " <>))

getStack :: AWS.Env env => InstanceSpec.Name env -> MIO env (Maybe CF.Stack)
getStack name =
  catchJust handleNotFoundError (pure <$> getExistingStack name) pure
  where
    handleNotFoundError :: Amazonka.Error -> Maybe (Maybe CF.Stack)
    handleNotFoundError
      (Amazonka.ServiceError
        Amazonka.ServiceError'
        { _serviceErrorCode    = Amazonka.ErrorCode "ValidationError"
        , _serviceErrorMessage = Just actualMessage
        }
      )
      = if actualMessage == expectedMessage
        then pure empty
        else empty
    handleNotFoundError _error = empty

    expectedMessage :: Amazonka.ErrorMessage
    expectedMessage =
      Amazonka.ErrorMessage $ "Stack with id " <> toText name <> " does not exist"

getStackId :: AWS.Env env => InstanceSpec.Name env -> MIO env (Maybe Id)
getStackId = getId <=< getStack
  where
    getId :: Maybe CF.Stack -> MIO env (Maybe Id)
    getId = maybe (pure empty) ((pure <$>) . idFromStack)

getExistingStack
  :: forall env . AWS.Env env
  => InstanceSpec.Name env
  -> MIO env CF.Stack
getExistingStack name = maybe failMissingRequested pure =<< doRequest
  where
    doRequest :: MIO env (Maybe CF.Stack)
    doRequest = runConduit
      $ AWS.listResource describeSpecificStack (fromMaybe [] . getField @"stacks")
      .| find ((toText name ==) . getField @"stackName")

    failMissingRequested :: MIO env a
    failMissingRequested
      = throwString
      $ "Successful request to stack " <> convertText name <> " did not return the stack"

    describeSpecificStack :: CF.DescribeStacks
    describeSpecificStack = set CF.describeStacks_stackName (pure $ toText name) CF.newDescribeStacks

getExistingStackId
  :: AWS.Env env
  => InstanceSpec.Name env
  -> MIO env Id
getExistingStackId = idFromStack <=< getExistingStack

getOutput :: AWS.Env env => InstanceSpec.Name env -> Text -> MIO env Text
getOutput name key = do
  stack <- getExistingStack name

  maybe
    (failStack $ "Output " <> convertText key <> " missing")
    (maybe (failStack $ "Output " <> convertText key <> " has no value") pure . getField @"outputValue")
    (Foldable.find ((==) (pure key) . getField @"outputKey") (fromMaybe [] $ getField @"outputs" stack))
  where
    failStack :: Text -> MIO env a
    failStack message
      = throwString . convertText $ "Stack: " <> convertText name <> " " <> message

stackNames :: AWS.Env env => ConduitT () (InstanceSpec.Name env) (MIO env) ()
stackNames
  =  AWS.listResource CF.newDescribeStacks (fromMaybe [] . getField @"stacks")
  .| map (InstanceSpec.mkName . getField @"stackName")

prepareOperation
  :: forall env a . (AWS.Env env, StackDeploy.Env env)
  => OperationFields a
  -> InstanceSpec env
  -> Token
  -> a
  -> MIO env a
prepareOperation OperationFields{..} InstanceSpec{..} token
  = setTemplateBody
  . set     capabilitiesField (pure capabilities)
  . set     parametersField   (pure $ Parameters.cfParameters parameters)
  . set     roleARNField      (toText <$> roleARN)
  . setText tokenField        token
  where
    setTemplateBody :: a -> MIO env a
    setTemplateBody request =
      if BS.length templateBodyBS <= maxBytes
        then pure $ setText templateBodyField templateBody request
        else s3Template request

    s3Template :: a -> MIO env a
    s3Template request = do
      ask >>=
        (maybe failMissingTemplateBucket (doUpload request =<<) . (.getTemplateBucketName)) . getField @"stackDeployConfig"

    doUpload :: a -> S3.BucketName -> MIO env a
    doUpload request bucketName@(S3.BucketName bucketNameText) = do
      S3.syncTarget targetObject
      pure $ setText templateURLField s3URL request
      where
        s3URL = "https://" <> bucketNameText <> ".s3.amazonaws.com/" <> S3.targetObjectKeyText targetObject

        targetObject =
          (S3.hashedTargetObject bucketName (toText name) "json" templateBodyBS)
            { S3.uploadCallback = liftIO . Text.putStrLn . ("Uploading template: " <>)
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
    templateBodyBS = LBS.toStrict $ Template.encode template

setText :: (Applicative f, ToText b) => Lens' a (f Text) -> b -> a -> a
setText field value = set field (pure $ toText value)

finalMessage :: RemoteOperationResult -> Text
finalMessage = \case
  RemoteOperationFailure -> "failure"
  RemoteOperationSuccess -> "succcess"

idFromStack :: CF.Stack -> MIO env Id
idFromStack = accessStackId CF.stack_stackId

accessStackId :: Lens' a (Maybe Text) -> a -> MIO env Id
accessStackId lens
  = maybe
     (throwString "Remote stack without stack id")
     (pure . Id)
  . view lens
