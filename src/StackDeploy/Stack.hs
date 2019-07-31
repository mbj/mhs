module StackDeploy.Stack
  ( finalMessage
  , getExistingStack
  , getOutput
  , getStackId
  , perform
  , printEvent
  , stackNames
  )
where

import Control.Exception.Base (AssertionFailed(AssertionFailed))
import Control.Lens (Lens', set, view)
import Control.Monad ((<=<))
import Control.Monad.Catch (catchIf, throwM)
import Control.Monad.Trans.AWS (AWSConstraint)
import Data.ByteString.Lazy (toStrict)
import Data.Conduit (ConduitT, (.|), runConduit)
import Data.Conduit.Combinators (find, map)
import Data.Maybe (Maybe(Just), fromMaybe)
import Data.String (String)
import Data.Text (unwords)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Network.AWS
import Network.AWS.CloudFormation.CreateStack
import Network.AWS.CloudFormation.DeleteStack
import Network.AWS.CloudFormation.DescribeStacks
import Network.AWS.CloudFormation.Types hiding (stack)
import Network.AWS.CloudFormation.UpdateStack
import StackDeploy.AWS
import StackDeploy.IO
import StackDeploy.Prelude
import StackDeploy.Template
import StackDeploy.Types
import StackDeploy.Wait
import Stratosphere (Template)

import qualified Data.Foldable as Foldable

data OperationFields a = OperationFields
  { tokenField        :: Lens' a (Maybe Text)
  , capabilitiesField :: Lens' a [Capability]
  , parametersField   :: Lens' a [Parameter]
  , roleARNField      :: Lens' a (Maybe Text)
  , templateBodyField :: Lens' a (Maybe Text)
  }

perform
  :: forall m r . (AWSConstraint r m, MonadAWS m)
  => Operation
  -> m RemoteOperationResult
perform = \case
  (OpCreate name instanceSpec template) ->
    successCallback instanceSpec =<<
      create name instanceSpec template
  (OpDelete stackId) ->
    runStackId stackId delete
  (OpUpdate stackId instanceSpec template) ->
    successCallback instanceSpec =<<
      runStackId stackId (update instanceSpec template)
  where
    runStackId
      :: Id
      -> (RemoteOperation -> m RemoteOperationResult)
      -> m RemoteOperationResult
    runStackId stackId action = do
      token <- newToken
      action RemoteOperation{..}

    create :: Name -> InstanceSpec -> Template -> m RemoteOperationResult
    create name instanceSpec@InstanceSpec{..} template = do
      void prepareSync
      token   <- newToken
      stackId <- getId =<< doCreate token
      waitFor RemoteOperation{..}
      where
        getId :: Rs CreateStack -> m Id
        getId =
          maybe
            (throwM $ AssertionFailed "Remote stack without stack id")
            (pure . Id)
          . view csrsStackId

        doCreate :: Token -> m (Rs CreateStack)
        doCreate token
          = send
          . configureStack template operationFields instanceSpec token
          . createStack
          $ toText name

        operationFields = OperationFields
          { capabilitiesField = csCapabilities
          , parametersField   = csParameters
          , roleARNField      = csRoleARN
          , templateBodyField = csTemplateBody
          , tokenField        = csClientRequestToken
          }

    delete :: RemoteOperation -> m RemoteOperationResult
    delete remoteOperation@RemoteOperation{..} =
      doDelete >> waitFor remoteOperation
      where
        doDelete :: m ()
        doDelete
          = void
          . send
          . setText dsClientRequestToken token
          . deleteStack $ toText stackId

    update
      :: InstanceSpec
      -> Template
      -> RemoteOperation
      -> m RemoteOperationResult
    update
      instanceSpec@InstanceSpec{..}
      template
      remoteOperation@RemoteOperation{..} = do
        void prepareSync
        catchIf isNoUpdateError
          (doUpdate >> waitFor remoteOperation)
          (const $ pure RemoteOperationSuccess)
      where
        doUpdate :: m ()
        doUpdate = void
          . send
          . configureStack template operationFields instanceSpec token
          . updateStack
          $ toText stackId

        isNoUpdateError
          ( ServiceError
            ServiceError'
            { _serviceCode =
               ErrorCode "ValidationError"
            , _serviceMessage =
              Just (ErrorMessage "No updates are to be performed.")
            }
          ) = True

        isNoUpdateError _ = False

        operationFields = OperationFields
          { capabilitiesField = usCapabilities
          , parametersField   = usParameters
          , roleARNField      = usRoleARN
          , templateBodyField = usTemplateBody
          , tokenField        = usClientRequestToken
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

printEvent :: forall m . MonadIO m => StackEvent -> m ()
printEvent event = do
  say $ unwords
    [ timestamp
    , physicalResourceId
    , logicalResourceId
    , resourceType
    , resourceStatus
    ]
  sayReason $ view seResourceStatusReason event
  where
    logicalResourceId =
      fromMaybe
        "[unknown-logical-resource-id]"
        (view seLogicalResourceId event)

    physicalResourceId =
      fromMaybe
        "[unknown-pysical-resource-id]"
        (view sePhysicalResourceId event)

    resourceType =
      fromMaybe
        "[unknown-resource-type]"
        (view seResourceType event)

    resourceStatus :: Text
    resourceStatus =
      maybe
        "[unknown-resource-type]"
        (convertText . show)
        (view seResourceStatus event)

    timeFormat :: String
    timeFormat = "%Y-%m-%dT%H:%M:%S"

    timestamp :: Text
    timestamp
      = convertText
      . formatTime defaultTimeLocale timeFormat
      $ view seTimestamp event

    sayReason :: Maybe Text -> m ()
    sayReason = maybe (pure ()) (say . ("- " <>))

getStackId :: forall m . MonadAWS m => Name -> m (Maybe Id)
getStackId = getId <=< getStack
  where
    getId :: Maybe Stack -> m (Maybe Id)
    getId = maybe (pure empty) ((pure <$>) . remoteId)

    remoteId :: Stack -> m Id
    remoteId value = maybe
      (throwM $ AssertionFailed "Remote stack without stack id")
      (pure . Id)
      (view sStackId value)

getExistingStack :: forall m . MonadAWS m => Name -> m Stack
getExistingStack name = maybe failMissingRequested pure =<< doRequest
  where
    doRequest :: m (Maybe Stack)
    doRequest = liftAWS . runConduit
      $  listResource describeSpecificStack dsrsStacks
      .| find ((toText name ==) . view sStackName)

    failMissingRequested :: m a
    failMissingRequested
      = throwM
      . AssertionFailed
      $ "Successful request to stack " <> convertText name <> " did not return the stack"

    describeSpecificStack :: DescribeStacks
    describeSpecificStack = set dStackName (pure $ toText name) describeStacks

getStack :: forall m . MonadAWS m => Name -> m (Maybe Stack)
getStack name =
  catchIf isNotFoundError (pure <$> getExistingStack name) (const $ pure empty)
  where
    isNotFoundError
      ( ServiceError
        ServiceError'
        { _serviceCode    = ErrorCode "ValidationError"
        , _serviceMessage = Just actualMessage
        }
      )
      = actualMessage == expectedMessage

    isNotFoundError _ = False

    expectedMessage :: ErrorMessage
    expectedMessage =
      ErrorMessage $ "Stack with id " <> toText name <> " does not exist"

stackNames :: (AWSConstraint r m, MonadAWS m) => ConduitT () Name m ()
stackNames =
  listResource describeStacks dsrsStacks .| map (Name . view sStackName)

configureStack
  :: Template
  -> OperationFields a
  -> InstanceSpec
  -> Token
  -> a
  -> a
configureStack template OperationFields{..} InstanceSpec{..} token
  = set     capabilitiesField capabilities
  . set     parametersField   parameters
  . set     roleARNField      (toText <$> roleARN)
  . setText templateBodyField templateBody
  . setText tokenField        token
  where
    templateBody = decodeUtf8 . toStrict $ encodeTemplate template

setText :: (Applicative f, ToText b) => Lens' a (f Text) -> b -> a -> a
setText field value = set field (pure $ toText value)

finalMessage :: RemoteOperationResult -> Text
finalMessage = \case
  RemoteOperationFailure -> "failure"
  RemoteOperationSuccess -> "succcess"

getOutput :: forall m . MonadAWS m => Name -> Text -> m Text
getOutput name key = do
  stack <- maybe
    (failStack "not found")
    pure
    =<< getStack name

  maybe
    (failStack $ "Output " <> convertText key <> " missing")
    (maybe (failStack $ "Output " <> convertText key <> " has no value") pure . view oOutputValue)
    (Foldable.find ((==) (pure key) . view oOutputKey) (view sOutputs stack))

  where
    failStack :: Text -> m a
    failStack message
      = liftIO . fail . convertText $ "Stack: " <> convertText name <> " " <> message
