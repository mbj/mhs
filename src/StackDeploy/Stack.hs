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

import Control.Exception.Base (AssertionFailed(AssertionFailed))
import Control.Lens (Lens', set, view)
import Data.Conduit (ConduitT, (.|), runConduit)
import Data.Conduit.Combinators (find, map)
import Data.String (String)
import Data.Text (Text)
import Data.Time.Format (defaultTimeLocale, formatTime)
import StackDeploy.AWS
import StackDeploy.IO
import StackDeploy.InstanceSpec (InstanceSpec(..))
import StackDeploy.Prelude
import StackDeploy.Types
import StackDeploy.Wait

import qualified Data.ByteString.Lazy                      as LBS
import qualified Data.Foldable                             as Foldable
import qualified Data.Text                                 as Text
import qualified Data.Text.Encoding                        as Text
import qualified Network.AWS                               as AWS
import qualified Network.AWS.CloudFormation.CreateStack    as CF
import qualified Network.AWS.CloudFormation.DeleteStack    as CF
import qualified Network.AWS.CloudFormation.DescribeStacks as CF
import qualified Network.AWS.CloudFormation.Types          as CF
import qualified Network.AWS.CloudFormation.UpdateStack    as CF
import qualified StackDeploy.InstanceSpec                  as InstanceSpec
import qualified StackDeploy.Parameters                    as Parameters
import qualified StackDeploy.Template                      as Template

data OperationFields a = OperationFields
  { tokenField        :: Lens' a (Maybe Text)
  , capabilitiesField :: Lens' a [CF.Capability]
  , parametersField   :: Lens' a [CF.Parameter]
  , roleARNField      :: Lens' a (Maybe Text)
  , templateBodyField :: Lens' a (Maybe Text)
  }

perform
  :: forall m r . (AWSConstraint r m, MonadAWS m)
  => Operation
  -> m RemoteOperationResult
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
      -> (RemoteOperation -> m RemoteOperationResult)
      -> m RemoteOperationResult
    runStackId stackId action = do
      token <- newToken
      action RemoteOperation{..}

    create :: InstanceSpec -> m RemoteOperationResult
    create instanceSpec@InstanceSpec{..} = do
      void prepareSync
      token   <- newToken
      stackId <- accessStackId CF.csrsStackId =<< doCreate token
      waitFor RemoteOperation{..}
      where
        doCreate :: Token -> m (AWS.Rs CF.CreateStack)
        doCreate token
          = AWS.send
          . configureStack operationFields instanceSpec token
          . CF.createStack
          $ toText name

        operationFields = OperationFields
          { capabilitiesField = CF.csCapabilities
          , parametersField   = CF.csParameters
          , roleARNField      = CF.csRoleARN
          , templateBodyField = CF.csTemplateBody
          , tokenField        = CF.csClientRequestToken
          }

    delete :: RemoteOperation -> m RemoteOperationResult
    delete remoteOperation@RemoteOperation{..} =
      doDelete >> waitFor remoteOperation
      where
        doDelete :: m ()
        doDelete
          = void
          . AWS.send
          . setText CF.dsClientRequestToken token
          . CF.deleteStack $ toText stackId

    update
      :: InstanceSpec
      -> RemoteOperation
      -> m RemoteOperationResult
    update
      instanceSpec@InstanceSpec{..}
      remoteOperation@RemoteOperation{..} = do
        void prepareSync
        catchIf isNoUpdateError
          (doUpdate >> waitFor remoteOperation)
          (const $ pure RemoteOperationSuccess)
      where
        doUpdate :: m ()
        doUpdate = void
          . AWS.send
          . configureStack operationFields instanceSpec token
          . CF.updateStack
          $ toText stackId

        isNoUpdateError
          ( AWS.ServiceError
            AWS.ServiceError'
            { _serviceCode =
               AWS.ErrorCode "ValidationError"
            , _serviceMessage =
              Just (AWS.ErrorMessage "No updates are to be performed.")
            }
          ) = True
        isNoUpdateError _ = False

        operationFields = OperationFields
          { capabilitiesField = CF.usCapabilities
          , parametersField   = CF.usParameters
          , roleARNField      = CF.usRoleARN
          , templateBodyField = CF.usTemplateBody
          , tokenField        = CF.usClientRequestToken
          }

    waitFor :: RemoteOperation -> m RemoteOperationResult
    waitFor remoteOperation = waitForAccept remoteOperation printEvent

    successCallback
      :: InstanceSpec
      -> RemoteOperationResult
      -> m RemoteOperationResult
    successCallback InstanceSpec{..} result = case result of
      RemoteOperationSuccess -> onSuccess >> pure result
      _                      -> pure result

printEvent :: forall m . MonadIO m => CF.StackEvent -> m ()
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
        "[unknown-pysical-resource-id]"
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

    sayReason :: Maybe Text -> m ()
    sayReason = maybe (pure ()) (say . ("- " <>))

getStack :: forall m . MonadAWS m => InstanceSpec.Name -> m (Maybe CF.Stack)
getStack name =
  catchIf isNotFoundError (pure <$> getExistingStack name) (const $ pure empty)
  where
    isNotFoundError
      ( AWS.ServiceError
        AWS.ServiceError'
        { _serviceCode    = AWS.ErrorCode "ValidationError"
        , _serviceMessage = Just actualMessage
        }
      )
      = actualMessage == expectedMessage

    isNotFoundError _ = False

    expectedMessage :: AWS.ErrorMessage
    expectedMessage =
      AWS.ErrorMessage $ "Stack with id " <> toText name <> " does not exist"

getStackId :: forall m . MonadAWS m => InstanceSpec.Name -> m (Maybe Id)
getStackId = getId <=< getStack
  where
    getId :: Maybe CF.Stack -> m (Maybe Id)
    getId = maybe (pure empty) ((pure <$>) . idFromStack)

getExistingStack :: forall m . MonadAWS m => InstanceSpec.Name -> m CF.Stack
getExistingStack name = maybe failMissingRequested pure =<< doRequest
  where
    doRequest :: m (Maybe CF.Stack)
    doRequest = AWS.liftAWS . runConduit
      $  listResource describeSpecificStack CF.dsrsStacks
      .| find ((toText name ==) . view CF.sStackName)

    failMissingRequested :: m a
    failMissingRequested
      = throwM
      . AssertionFailed
      $ "Successful request to stack " <> convertText name <> " did not return the stack"

    describeSpecificStack :: CF.DescribeStacks
    describeSpecificStack = set CF.dStackName (pure $ toText name) CF.describeStacks

getExistingStackId
  :: forall m r . (AWSConstraint r m, MonadAWS m)
  => InstanceSpec.Name
  -> m Id
getExistingStackId = idFromStack <=< getExistingStack

getOutput :: forall m . MonadAWS m => InstanceSpec.Name -> Text -> m Text
getOutput name key = do
  stack <- getExistingStack name

  maybe
    (failStack $ "Output " <> convertText key <> " missing")
    (maybe (failStack $ "Output " <> convertText key <> " has no value") pure . view CF.oOutputValue)
    (Foldable.find ((==) (pure key) . view CF.oOutputKey) (view CF.sOutputs stack))

  where
    failStack :: Text -> m a
    failStack message
      = liftIO . fail . convertText $ "Stack: " <> convertText name <> " " <> message

stackNames :: (AWSConstraint r m, MonadAWS m) => ConduitT () InstanceSpec.Name m ()
stackNames =
  listResource CF.describeStacks CF.dsrsStacks .| map (InstanceSpec.Name . view CF.sStackName)

configureStack
  :: OperationFields a
  -> InstanceSpec
  -> Token
  -> a
  -> a
configureStack OperationFields{..} InstanceSpec{..} token
  = set     capabilitiesField capabilities
  . set     parametersField   (Parameters.cfParameters parameters)
  . set     roleARNField      (toText <$> roleARN)
  . setText templateBodyField templateBody
  . setText tokenField        token
  where
    templateBody
      = Text.decodeUtf8
      . LBS.toStrict
      $ Template.encode template

setText :: (Applicative f, ToText b) => Lens' a (f Text) -> b -> a -> a
setText field value = set field (pure $ toText value)

finalMessage :: RemoteOperationResult -> Text
finalMessage = \case
  RemoteOperationFailure -> "failure"
  RemoteOperationSuccess -> "succcess"

idFromStack :: MonadThrow m => CF.Stack -> m Id
idFromStack = accessStackId CF.sStackId

accessStackId :: MonadThrow m => Lens' a (Maybe Text) -> a -> m Id
accessStackId lens
  = maybe
     (throwM $ AssertionFailed "Remote stack without stack id")
     (pure . Id)
  . view lens
