module StackDeploy.Stack
  ( fetchStackOutput
  , readCloudFormationStack
  , readCloudFormationStackPresent
  , readExistingStack
  , readExistingStackPresent
  , readIdFromStack
  , readInstanceOutput
  , readStackIdField
  )
where

import Control.Lens (Lens', set, view)
import Data.Conduit ((.|))
import StackDeploy.Prelude
import StackDeploy.Types

import qualified Amazonka.CloudFormation.DescribeStacks as CF
import qualified Amazonka.CloudFormation.Types          as CF
import qualified Data.Conduit                           as Conduit
import qualified Data.Conduit.Combinators               as Conduit
import qualified Data.Foldable                          as Foldable
import qualified MIO.Amazonka                           as AWS
import qualified StackDeploy.AWS                        as AWS
import qualified StackDeploy.InstanceSpec               as StackDeploy
import qualified Stratosphere                           as CFT

fetchStackOutput
  :: forall m . MonadIO m
  => CF.Stack
  -> CFT.Output
  -> m Text
fetchStackOutput stack cftOutput =
  maybe
    (failOutputKey "missing")
    (maybe (failOutputKey "has no value") pure . (.outputValue))
    $ Foldable.find
      ((==) (pure key) . (.outputKey))
      (fromMaybe [] stack.outputs)
  where
    key :: Text
    key = cftOutput.name

    failOutputKey :: Text -> m a
    failOutputKey message
      = failStack
      $ "Output: " <> convertText key <> " " <> message

    failStack :: Text -> m a
    failStack message
      = throwString
      . convertText
      $ "Stack: " <> stack.stackName <> " " <> message

readInstanceOutput
  :: AWS.Env env
  => StackDeploy.InstanceName
  -> CFT.Output
  -> MIO env Text
readInstanceOutput instanceName cftOutput = do
  stack <- readCloudFormationStackPresent instanceName
  fetchStackOutput stack cftOutput

readExistingStack
  :: AWS.Env env
  => StackDeploy.InstanceName
  -> MIO env (Maybe ExistingStack)
readExistingStack name =
  traverse present =<< readCloudFormationStack name
  where
    present :: CF.Stack -> MIO env ExistingStack
    present stack = do
      stackId <- readIdFromStack stack
      pure ExistingStack
        { stackId    = stackId
        , outputs    = fromMaybe [] stack.outputs
        , parameters = fromMaybe [] stack.parameters
        }

readExistingStackPresent
  :: AWS.Env env
  => StackDeploy.InstanceName
  -> MIO env ExistingStack
readExistingStackPresent instanceName =
  maybe (absent instanceName) pure =<< readExistingStack instanceName

readCloudFormationStack
  :: forall env . AWS.Env env
  => StackDeploy.InstanceName
  -> MIO env (Maybe CF.Stack)
readCloudFormationStack name
  = Conduit.runConduit
  $  AWS.nestedResourceC describeSpecificStack (fromMaybe [] . (.stacks))
  .| Conduit.find ((convert name ==) . (.stackName))
  where
    describeSpecificStack :: CF.DescribeStacks
    describeSpecificStack =
      set CF.describeStacks_stackName (pure $ convert name) CF.newDescribeStacks

readCloudFormationStackPresent
  :: forall env . AWS.Env env
  => StackDeploy.InstanceName
  -> MIO env CF.Stack
readCloudFormationStackPresent instanceName =
  maybe (absent instanceName) pure =<< readCloudFormationStack instanceName

readIdFromStack :: CF.Stack -> MIO env StackId
readIdFromStack = readStackIdField CF.stack_stackId

readStackIdField :: Lens' a (Maybe Text) -> a -> MIO env StackId
readStackIdField lens
  = maybe (throwString "stack without stack id") convertThrow . view lens

absent :: MonadIO m => StackDeploy.InstanceName -> m a
absent instanceName = throwString $ "Stack does not exist: " <> convertVia @Text instanceName
