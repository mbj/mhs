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
import StackDeploy.Environment
import StackDeploy.IO
import StackDeploy.InstanceSpec (InstanceSpec(..))
import StackDeploy.Prelude
import StackDeploy.Types
import StackDeploy.Wait

import qualified Data.ByteString                           as BS
import qualified Data.ByteString.Lazy                      as LBS
import qualified Data.Foldable                             as Foldable
import qualified Data.Text                                 as Text
import qualified Data.Text.Encoding                        as Text
import qualified Data.Text.IO                              as Text
import qualified Network.AWS.CloudFormation.CreateStack    as CF
import qualified Network.AWS.CloudFormation.DeleteStack    as CF
import qualified Network.AWS.CloudFormation.DescribeStacks as CF
import qualified Network.AWS.CloudFormation.Types          as CF
import qualified Network.AWS.CloudFormation.UpdateStack    as CF
import qualified Network.AWS.S3.Types                      as S3
import qualified StackDeploy.AWS                           as AWS
import qualified StackDeploy.InstanceSpec                  as InstanceSpec
import qualified StackDeploy.Parameters                    as Parameters
import qualified StackDeploy.S3                            as S3
import qualified StackDeploy.Template                      as Template

data OperationFields a = OperationFields
  { tokenField        :: Lens' a (Maybe Text)
  , capabilitiesField :: Lens' a [CF.Capability]
  , parametersField   :: Lens' a [CF.Parameter]
  , roleARNField      :: Lens' a (Maybe Text)
  , templateBodyField :: Lens' a (Maybe Text)
  , templateURLField  :: Lens' a (Maybe Text)
  }

perform
  :: forall env . (HasAWS env, HasEnvironment env)
  => Operation
  -> RIO env RemoteOperationResult
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
      -> (RemoteOperation -> RIO env RemoteOperationResult)
      -> RIO env RemoteOperationResult
    runStackId stackId action = do
      token <- newToken
      action RemoteOperation{..}

    create :: InstanceSpec -> RIO env RemoteOperationResult
    create instanceSpec@InstanceSpec{..} = do
      token   <- newToken
      stackId <- accessStackId CF.csrsStackId =<< doCreate token
      waitFor RemoteOperation{..}
      where
        doCreate :: Token -> RIO env (AWS.Rs CF.CreateStack)
        doCreate token =
          prepareOperation operationFields instanceSpec token (CF.createStack $ toText name)
            >>= AWS.send

        operationFields = OperationFields
          { capabilitiesField = CF.csCapabilities
          , parametersField   = CF.csParameters
          , roleARNField      = CF.csRoleARN
          , templateBodyField = CF.csTemplateBody
          , templateURLField  = CF.csTemplateURL
          , tokenField        = CF.csClientRequestToken
          }

    delete :: RemoteOperation -> RIO env RemoteOperationResult
    delete remoteOperation@RemoteOperation{..} =
      doDelete >> waitFor remoteOperation
      where
        doDelete :: RIO env ()
        doDelete
          = void
          . AWS.send
          . setText CF.dsClientRequestToken token
          . CF.deleteStack $ toText stackId

    update
      :: InstanceSpec
      -> RemoteOperation
      -> RIO env RemoteOperationResult
    update
      instanceSpec@InstanceSpec{..}
      remoteOperation@RemoteOperation{..} =
        catchJust testNoUpdateError
          (doUpdate >> waitFor remoteOperation)
          (const $ pure RemoteOperationSuccess)
      where
        doUpdate :: RIO env ()
        doUpdate =
          prepareOperation operationFields instanceSpec token (CF.updateStack $ toText stackId)
            >>= void . AWS.send

        testNoUpdateError :: AWS.Error -> Maybe AWS.Error
        testNoUpdateError
          ( AWS.ServiceError
            AWS.ServiceError'
            { _serviceCode =
               AWS.ErrorCode "ValidationError"
            , _serviceMessage =
              Just (AWS.ErrorMessage "No updates are to be performed.")
            }
          ) = empty
        testNoUpdateError error = pure error

        operationFields = OperationFields
          { capabilitiesField = CF.usCapabilities
          , parametersField   = CF.usParameters
          , roleARNField      = CF.usRoleARN
          , templateBodyField = CF.usTemplateBody
          , templateURLField  = CF.usTemplateURL
          , tokenField        = CF.usClientRequestToken
          }

    waitFor :: RemoteOperation -> RIO env RemoteOperationResult
    waitFor remoteOperation = waitForAccept remoteOperation printEvent

    successCallback
      :: InstanceSpec
      -> RemoteOperationResult
      -> RIO env RemoteOperationResult
    successCallback InstanceSpec{..} result = case result of
      RemoteOperationSuccess -> onSuccess >> pure result
      _                      -> pure result

printEvent :: CF.StackEvent -> RIO env ()
printEvent event = do
  say $ Text.unwords
    [ timestamp
    , physicalResourceId
    , logicalResourceId
    , resourceType
    , resourceStatus
    ]
  sayReason $ view CF.seResourceStatusReason event
  where
    logicalResourceId =
      fromMaybe
        "[unknown-logical-resource-id]"
        (view CF.seLogicalResourceId event)

    physicalResourceId =
      fromMaybe
        "[unknown-physical-resource-id]"
        (view CF.sePhysicalResourceId event)

    resourceType =
      fromMaybe
        "[unknown-resource-type]"
        (view CF.seResourceType event)

    resourceStatus :: Text
    resourceStatus =
      maybe
        "[unknown-resource-type]"
        (convertText . show)
        (view CF.seResourceStatus event)

    timeFormat :: String
    timeFormat = "%Y-%m-%dT%H:%M:%S"

    timestamp :: Text
    timestamp
      = convertText
      . formatTime defaultTimeLocale timeFormat
      $ view CF.seTimestamp event

    sayReason :: Maybe Text -> RIO env ()
    sayReason = maybe (pure ()) (say . ("- " <>))

getStack :: HasAWS env => InstanceSpec.Name -> RIO env (Maybe CF.Stack)
getStack name =
  catchJust testNotFoundError (pure <$> getExistingStack name) (const $ pure empty)
  where
    testNotFoundError :: AWS.Error -> Maybe AWS.Error
    testNotFoundError
      error@(AWS.ServiceError
        AWS.ServiceError'
        { _serviceCode    = AWS.ErrorCode "ValidationError"
        , _serviceMessage = Just actualMessage
        }
      )
      = if actualMessage == expectedMessage
        then empty
        else pure error
    testNotFoundError error = pure error

    expectedMessage :: AWS.ErrorMessage
    expectedMessage =
      AWS.ErrorMessage $ "Stack with id " <> toText name <> " does not exist"

getStackId :: HasAWS env => InstanceSpec.Name -> RIO env (Maybe Id)
getStackId = getId <=< getStack
  where
    getId :: Maybe CF.Stack -> RIO env (Maybe Id)
    getId = maybe (pure empty) ((pure <$>) . idFromStack)

getExistingStack :: forall env . HasAWS env => InstanceSpec.Name -> RIO env CF.Stack
getExistingStack name = maybe failMissingRequested pure =<< doRequest
  where
    doRequest :: RIO env (Maybe CF.Stack)
    doRequest = runConduit
      $ AWS.listResource describeSpecificStack CF.dsrsStacks
      .| find ((toText name ==) . view CF.sStackName)

    failMissingRequested :: RIO env a
    failMissingRequested
      = throwString
      $ "Successful request to stack " <> convertText name <> " did not return the stack"

    describeSpecificStack :: CF.DescribeStacks
    describeSpecificStack = set CF.dStackName (pure $ toText name) CF.describeStacks

getExistingStackId
  :: HasAWS env
  => InstanceSpec.Name
  -> RIO env Id
getExistingStackId = idFromStack <=< getExistingStack

getOutput :: HasAWS env => InstanceSpec.Name -> Text -> RIO env Text
getOutput name key = do
  stack <- getExistingStack name

  maybe
    (failStack $ "Output " <> convertText key <> " missing")
    (maybe (failStack $ "Output " <> convertText key <> " has no value") pure . view CF.oOutputValue)
    (Foldable.find ((==) (pure key) . view CF.oOutputKey) (view CF.sOutputs stack))

  where
    failStack :: Text -> RIO env a
    failStack message
      = throwString . convertText $ "Stack: " <> convertText name <> " " <> message

stackNames :: HasAWS env => ConduitT () InstanceSpec.Name (RIO env) ()
stackNames
  =  AWS.listResource CF.describeStacks CF.dsrsStacks
  .| map (InstanceSpec.mkName . view CF.sStackName)

prepareOperation
  :: forall env a . (HasAWS env, HasEnvironment env)
  => OperationFields a
  -> InstanceSpec
  -> Token
  -> a
  -> RIO env a
prepareOperation OperationFields{..} InstanceSpec{..} token
  = setTemplateBody
  . set     capabilitiesField capabilities
  . set     parametersField   (Parameters.cfParameters parameters)
  . set     roleARNField      (toText <$> roleARN)
  . setText tokenField        token
  where

    setTemplateBody :: a -> RIO env a
    setTemplateBody request =
      if BS.length templateBodyBS <= maxBytes
        then pure $ setText templateBodyField templateBody request
        else s3Template request

    s3Template :: a -> RIO env a
    s3Template request = do
      (getEnvironment <$> ask)
        >>= maybe failMissingTemplateBucket (doUpload request =<<) . getTemplateBucketName

    doUpload :: a -> S3.BucketName -> RIO env a
    doUpload request bucketName@(S3.BucketName bucketNameText) = do
      S3.syncTarget targetObject
      pure $ setText templateURLField s3URL request
      where
        s3URL = "https://" <> bucketNameText <> ".s3.amazonaws.com/" <> S3.targetObjectKeyText targetObject

        targetObject =
          (S3.hashedTargetObject bucketName (toText name) "json" templateBodyBS)
            { S3.uploadCallback = liftIO . Text.putStrLn . ("Uploading template: " <>)
            }

    failMissingTemplateBucket :: RIO env a
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

idFromStack :: CF.Stack -> RIO env Id
idFromStack = accessStackId CF.sStackId

accessStackId :: Lens' a (Maybe Text) -> a -> RIO env Id
accessStackId lens
  = maybe
     (throwString "Remote stack without stack id")
     (pure . Id)
  . view lens
