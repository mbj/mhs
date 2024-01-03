module AWS.TemporaryIngressRule
  ( Env
  , IngressConfig(..)
  , StackConfig(..)
  , WithTemporaryIngressRule
  , bootLambdaExpire
  , component
  , parserInfo
  , withTemporaryIngressRule
  )
where

import AWS.TemporaryIngressRule.Prelude
import CLI.Utils
import Control.Lens ((.~), (?~))
import Data.Conduit (ConduitT, (.|))
import Text.Read (read)

import qualified AWS.Checkip
import qualified AWS.Lambda.Runtime
import qualified Amazonka
import qualified Amazonka.CloudFormation.Types              as CloudFormation
import qualified Amazonka.EC2.AuthorizeSecurityGroupIngress as EC2
import qualified Amazonka.EC2.CreateTags                    as EC2
import qualified Amazonka.EC2.DescribeSecurityGroupRules    as EC2
import qualified Amazonka.EC2.RevokeSecurityGroupIngress    as EC2
import qualified Amazonka.EC2.Types                         as EC2
import qualified Data.Aeson                                 as JSON
import qualified Data.Conduit                               as Conduit
import qualified Data.Conduit.Combinators                   as Conduit
import qualified Data.Foldable                              as Foldable
import qualified Data.Text                                  as Text
import qualified Data.Text.IO                               as IO
import qualified Data.Time                                  as Time
import qualified Data.Time.Format.ISO8601                   as Time
import qualified MIO.Amazonka                               as AWS
import qualified MIO.Log                                    as Log
import qualified Network.HTTP.Client                        as HTTP
import qualified Network.HTTP.Client.TLS                    as HTTP
import qualified Network.HTTP.MClient                       as HTTP
import qualified Network.IP.Addr                            as Network
import qualified Options.Applicative                        as CLI
import qualified StackDeploy.CLI.Utils                      as StackDeploy.CLI
import qualified StackDeploy.Component                      as StackDeploy
import qualified StackDeploy.EnvSpec                        as StackDeploy
import qualified StackDeploy.InstanceSpec                   as StackDeploy
import qualified StackDeploy.Stack                          as StackDeploy
import qualified StackDeploy.Stratosphere                   as CFT
import qualified Stratosphere                               as CFT
import qualified Stratosphere.Events.Rule                   as Events
import qualified Stratosphere.IAM.Role                      as IAM
import qualified Stratosphere.Lambda.Function               as Lambda
import qualified Stratosphere.Lambda.Permission             as Lambda
import qualified System.Exit                                as System
import qualified System.IO                                  as System
import qualified UnliftIO.Exception                         as UnliftIO

type Env env = (AWS.Env env, HTTP.Env env, Log.Env env)

type NetAddr = Network.NetAddr Network.IP

data IngressConfig = IngressConfig
  { securityGroupIdOutput :: CFT.Output
  , portOutput            :: CFT.Output
  }
  deriving stock Show

type WithTemporaryIngressRule env a = CloudFormation.Stack -> MIO env a -> MIO env a

data Request = Request
  { cidr    :: Text
  , groupId :: Text
  , netAddr :: NetAddr
  , now     :: Time.UTCTime
  , port    :: Network.InetPort
  }

formatRequest :: Request -> Text
formatRequest Request{..} = groupId <> " cidr: " <> cidr <> " port: " <> convert (show port)

withTemporaryIngressRule :: Env env => IngressConfig -> WithTemporaryIngressRule env a
withTemporaryIngressRule config stack action = authorizeIngressRule config stack >> action

authorizeIngressRule :: forall env . Env env => IngressConfig -> CloudFormation.Stack -> MIO env ()
authorizeIngressRule IngressConfig{..} stack = do
  request <- readRequest
  maybe (createIngressRule request) (updateIngressRuleLastSeen request) =<< findIngressRule request
  where
    readRequest :: MIO env Request
    readRequest = do
      netAddr <- AWS.Checkip.readNetAddr
      now     <- liftIO Time.getCurrentTime
      groupId <- liftIO (StackDeploy.fetchStackOutput stack securityGroupIdOutput)
      port    <- read . convert <$> liftIO (StackDeploy.fetchStackOutput stack portOutput)
      pure Request{cidr = convert @Text @String $ Network.printNetAddr netAddr, ..}

revokeExpiredIngressRules :: forall env . Env env => [Text] -> MIO env ()
revokeExpiredIngressRules groupIds =
  Conduit.runConduit $ expiredIngressRulesC groupIds .| Conduit.mapM_ revoke
  where
    revoke :: EC2.SecurityGroupRule -> MIO env ()
    revoke rule = do
      groupId             <- require (.groupId)
      securityGroupRuleId <- require (.securityGroupRuleId)

      Log.info $ "Removing rule: " <> formatSecurityGroupRule rule

      void . AWS.send
        $ EC2.newRevokeSecurityGroupIngress
        & EC2.revokeSecurityGroupIngress_groupId              ?~ groupId
        & EC2.revokeSecurityGroupIngress_securityGroupRuleIds ?~ [securityGroupRuleId]
      where
        require access = maybe (UnliftIO.throwString "incoherent rule") pure (access rule)

listExpiredIngressRules :: forall env . Env env => [Text] -> MIO env ()
listExpiredIngressRules groupIds = do
  Conduit.runConduit
    $  expiredIngressRulesC groupIds
    .| Conduit.mapM_ (liftIO . IO.putStrLn . formatSecurityGroupRule)

formatSecurityGroupRule :: EC2.SecurityGroupRule -> Text
formatSecurityGroupRule rule
  =  field "cidr" (.cidrIpv4)
  <> field "cidr" (.cidrIpv6)
  <> field "port" (fmap (convert . show) . (.fromPort))
  where
    field :: Text -> (EC2.SecurityGroupRule -> Maybe Text) -> Text
    field label access = maybe "" (\value -> label <> ": " <> value <> " ") (access rule)

data TemporaryIngressRule = TemporaryIngressRule
  { lastSeenAt        :: Time.UTCTime
  , securityGroupRule :: EC2.SecurityGroupRule
  }

temporaryIngressRulesC
  :: forall env . Env env
  => [Text]
  -> ConduitT () TemporaryIngressRule (MIO env) ()
temporaryIngressRulesC groupIds = do
  AWS.paginate mkRequest
    .| Conduit.concatMap (fromMaybe [] . (.securityGroupRules))
    .| Conduit.mapM parseRule
  where
    mkRequest
      = EC2.newDescribeSecurityGroupRules
      & EC2.describeSecurityGroupRules_filters ?~ [mkGroupIdsFilter groupIds, mkTagFilter managedTag]

    parseRule securityGroupRule = do
      lastSeenAt <-
         maybe
           (UnliftIO.throwString "managed rule without last seen tag")
           parseLastSeen
           (Foldable.find ((lastSeenTagKey ==) . (.key)) (fromMaybe [] securityGroupRule.tags))

      pure TemporaryIngressRule{..}

    parseLastSeen :: EC2.Tag -> MIO env Time.UTCTime
    parseLastSeen
      = maybe (UnliftIO.throwString "manage rule with invalid last seen at tag value") pure
      . Time.iso8601ParseM
      . convert
      . (.value)

expiredIngressRulesC
  :: forall env . Env env
  => [Text]
  -> ConduitT () EC2.SecurityGroupRule (MIO env) ()
expiredIngressRulesC groupIds = do
   now <- liftIO Time.getCurrentTime

   temporaryIngressRulesC groupIds
    .| Conduit.filter (isStale now)
    .| Conduit.map (.securityGroupRule)
  where
    isStale now TemporaryIngressRule{..} = Time.diffUTCTime now lastSeenAt > maxStaleTime

    maxStaleTime :: Time.NominalDiffTime
    maxStaleTime = 3600 -- 1 hour

findIngressRule :: Env env => Request -> MIO env (Maybe EC2.SecurityGroupRule)
findIngressRule Request{..}
  = fmap listToMaybe
  . Conduit.runConduit
  $ AWS.paginate mkRequest
  .| Conduit.concatMap (fromMaybe [] . (.securityGroupRules))
  .| Conduit.sinkList
  where
    mkRequest
      = EC2.newDescribeSecurityGroupRules
      & EC2.describeSecurityGroupRules_filters ?~
        [ mkGroupIdsFilter [groupId]
        , mkTagFilter $ mkCIDRTag cidr
        , mkTagFilter $ mkPortTag port
        ]

updateIngressRuleLastSeen :: Env env => Request -> EC2.SecurityGroupRule -> MIO env ()
updateIngressRuleLastSeen request rule = do
  securityGroupRuleId <- required rule.securityGroupRuleId
  tags                <- updateLastSeenTag =<< required rule.tags

  Log.info $ "Updating expiry of ingress rule: " <> formatRequest request

  void
    . AWS.send
    $ EC2.newCreateTags
    & EC2.createTags_resources .~ [securityGroupRuleId]
    & EC2.createTags_tags      .~ tags
  where
    required :: Maybe a -> MIO env a
    required = maybe (UnliftIO.throwString "incoeherent rule") pure

    updateLastSeenTag :: [EC2.Tag] -> MIO env [EC2.Tag]
    updateLastSeenTag tags = do
      now <- liftIO Time.getCurrentTime
      pure (updateTag now <$> tags)
      where
        updateTag now tag =
          if tag.key == lastSeenTagKey
            then mkLastSeenTag now
            else tag

createIngressRule :: forall env . Env env => Request -> MIO env ()
createIngressRule request = do
  Log.info $ "Creating ingress rule: " <> formatRequest request
  either handleError (const $ pure ()) =<< AWS.sendEither (mkRequest request)
  where
    handleError :: Amazonka.Error -> MIO env ()
    handleError = \case
      (Amazonka.ServiceError
         Amazonka.ServiceError'
         {code = Amazonka.ErrorCode "InvalidPermission.Duplicate"}) ->
        Log.warn "Ingress rule already exists, must have been created concurrently"
      error -> UnliftIO.throwIO error

    mkRequest Request{..}
      = EC2.newAuthorizeSecurityGroupIngress
      & EC2.authorizeSecurityGroupIngress_groupId           ?~ groupId
      & EC2.authorizeSecurityGroupIngress_ipPermissions     ?~ [mkPermission]
      & EC2.authorizeSecurityGroupIngress_tagSpecifications ?~ [mkTagSpecification]
      where
        mkTagSpecification
          = EC2.newTagSpecification
          & EC2.tagSpecification_resourceType ?~ EC2.ResourceType_Security_group_rule
          & EC2.tagSpecification_tags         ?~ tags

        tags =
          [ managedTag
          , mkCIDRTag cidr
          , mkLastSeenTag now
          , mkPortTag port
          , mkTimeTag "CreatedAt" now
          ]

        mkPermission :: EC2.IpPermission
        mkPermission = setIpRange
          where
            raw
              = EC2.newIpPermission "tcp"
              & EC2.ipPermission_fromPort ?~ portInt
              & EC2.ipPermission_toPort   ?~ portInt

            portInt = convert $ Network.unInetPort port

            setIpRange
              = raw
              & case Network.toNetAddr46 netAddr of
                Network.IPv4{} -> EC2.ipPermission_ipRanges   ?~ [mkIpv4]
                Network.IPv6{} -> EC2.ipPermission_ipv6Ranges ?~ [mkIpv6]
              where
                mkIpv4
                  = EC2.newIpRange cidr
                  & EC2.ipRange_description ?~ description

                mkIpv6
                  = EC2.newIpv6Range
                  & EC2.ipv6Range_cidrIpv6    ?~ cidr
                  & EC2.ipv6Range_description ?~ description

            description :: Text
            description = "managed by aws-temporary-ingress-rule"


parserInfo :: forall env . Env env => [IngressConfig] -> CLI.ParserInfo (MIO env System.ExitCode)
parserInfo configs = CLI.info (CLI.helper <*> subcommands) CLI.idm
  where
    subcommands
      =  CLI.hsubparser
      $  mkCommand
           "config"
           (pure $ printIngressConfigurations $> System.ExitSuccess)
           "show configuration"
      <> mkCommand
           "authorize-all"
           (withStack authorizeAll <$> StackDeploy.CLI.instanceNameOption)
           "authorize all ingress configurations"
      <> mkCommand
           "revoke-expired"
           (runGroupIds revokeExpiredIngressRules <$> StackDeploy.CLI.instanceNameOption)
           "revoke expired ingress rules"
      <> mkCommand
           "list-expired"
           (runGroupIds listExpiredIngressRules <$> StackDeploy.CLI.instanceNameOption)
           "list expired ingress rules"

    printIngressConfigurations  = traverse_ (liftIO . IO.putStrLn . convert . show) configs

    withStack
      :: (CloudFormation.Stack -> MIO env ())
      -> StackDeploy.InstanceName
      -> MIO env System.ExitCode
    withStack action instanceName = do
      maybe absent present =<< StackDeploy.readCloudFormationStack instanceName
      where
        absent = UnliftIO.throwString $ "Stack does not exist: " <> convertVia @Text instanceName
        present stack = do
          action stack
          pure System.ExitSuccess

    runGroupIds
      :: ([Text] -> MIO env ())
      -> StackDeploy.InstanceName
      -> MIO env System.ExitCode
    runGroupIds action =
      withStack $ \stack -> do
        groupIds <- traverse
           (liftIO . StackDeploy.fetchStackOutput stack . (.securityGroupIdOutput)) configs
        action groupIds

    authorizeAll :: CloudFormation.Stack ->  MIO env ()
    authorizeAll stack = traverse_ (`authorizeIngressRule` stack) configs

mkTag :: Text -> Text -> EC2.Tag
mkTag suffix = EC2.newTag (mkTagKey suffix)

mkTimeTag :: Text -> Time.UTCTime -> EC2.Tag
mkTimeTag name time = mkTag name (convert $ Time.iso8601Show time)

mkTagKey :: Text -> Text
mkTagKey = ("temporary-ingress-rule-" <>)

lastSeenAtSuffix :: Text
lastSeenAtSuffix = "LastSeenAt"

mkLastSeenTag :: Time.UTCTime-> EC2.Tag
mkLastSeenTag = mkTimeTag lastSeenAtSuffix

lastSeenTagKey :: Text
lastSeenTagKey = mkTagKey lastSeenAtSuffix

mkCIDRTag :: Text -> EC2.Tag
mkCIDRTag = mkTag "CIDR"

mkPortTag :: Network.InetPort -> EC2.Tag
mkPortTag port = mkTag "Port" (convert $ show port)

mkGroupIdsFilter :: [Text] -> EC2.Filter
mkGroupIdsFilter groupIds
  = EC2.newFilter "group-id"
  & EC2.filter_values ?~ groupIds

managedTag :: EC2.Tag
managedTag = mkTag "Managed" "true"

mkTagFilter :: EC2.Tag -> EC2.Filter
mkTagFilter tag
  = EC2.newFilter ("tag:" <> tag.key)
  & EC2.filter_values ?~ [tag.value]

data StackConfig = StackConfig
  { lambdaCode            :: Lambda.CodeProperty
  , lambdaFunctionHandler :: CFT.Value Text
  }

data Environment = Environment
  { awsEnv          :: Amazonka.Env
  , httpSendRequest :: HTTP.SendRequest
  , logAction       :: Log.Action
  , resourceMap     :: AWS.ResourceMap
  }

instance AWS.HasResourceMap Environment where
  resourceMap = (.resourceMap)

bootLambdaExpire :: [IngressConfig] -> IO ()
bootLambdaExpire ingressConfigurations = do
  withEnvironment $ do
    groupIds <- Text.split (== ',') <$> StackDeploy.readEnvSpecFromEnvironment (groupIdsEnvSpec ingressConfigurations)
    AWS.Lambda.Runtime.run (const $ revokeExpiredIngressRules groupIds $> JSON.Null)

withEnvironment :: MIO Environment a -> IO a
withEnvironment action = do
  httpManager <- HTTP.newTlsManagerWith . HTTP.managerSetProxy HTTP.noProxy $ HTTP.tlsManagerSettings
  awsLogger   <- Amazonka.newLogger Amazonka.Info System.stderr
  awsEnv      <- setup awsLogger <$> Amazonka.newEnvFromManager httpManager Amazonka.discover

  AWS.withResourceMap $ \resourceMap ->
    runMIO
      Environment
      { httpSendRequest = HTTP.mkManagerSendRequest httpManager
      , logAction       = Log.defaultCLIAction
      , ..
      }
      action
  where
    setup :: Amazonka.Logger -> Amazonka.Env -> Amazonka.Env
    setup logger env = env
      { Amazonka.logger = logger
      }

groupIdsEnvSpec :: [IngressConfig] -> StackDeploy.EnvSpec
groupIdsEnvSpec ingressConfigurations
  = StackDeploy.EnvSpec
    (fromType @"TEMPORARY_INGRESS_RULE_GROUP_IDS")
    (StackDeploy.EnvSpecStackOutput $ mkGroupIdsOutput ingressConfigurations)

mkGroupIdsOutput :: [IngressConfig] -> CFT.Output
mkGroupIdsOutput ingressConfigurations
  = CFT.mkOutput "TempoaryIngressRuleGroupIds"
  . CFT.Join ","
  . CFT.ValueList
  $ ((.securityGroupIdOutput.value) <$> ingressConfigurations)

component
  :: StackConfig
  -> [IngressConfig]
  -> StackDeploy.Component
component StackConfig{..} ingressConfigurations = mempty
  { StackDeploy.resources = [lambdaFunction, lambdaLogGroup, lambdaRole, lambdaPermission, eventsRule]
  , StackDeploy.outputs   = [mkGroupIdsOutput ingressConfigurations]
  }
  where
    prefix :: Text
    prefix = "TemporyIngressRuleExpiry"

    lambdaFunction
      = CFT.resource (prefix <> "LambdaFunction")
      $ Lambda.mkFunction lambdaCode (CFT.getAttArn lambdaRole)
      & CFT.set @"Environment"  (StackDeploy.envSpecLambdaEnvironment [groupIdsEnvSpec ingressConfigurations])
      & CFT.set @"FunctionName" lambdaFunctionName
      & CFT.set @"Handler"      lambdaFunctionHandler
      & CFT.set @"Runtime"      "provided.al2"
      & CFT.set @"Timeout"      (CFT.Literal 60)

    lambdaLogGroup
      = CFT.resource (prefix <> "LogGroup")
      $ CFT.mkLambdaLogGroup lambdaFunctionName

    lambdaRole
      = CFT.resource (prefix <> "LambdaRule")
      $ IAM.mkRole (CFT.assumeRole "lambda.amazonaws.com")
      & CFT.set @"Policies" [CFT.mkLambdaLogsPolicy lambdaFunctionName, ingressRulePolicy]

    lambdaFunctionName :: CFT.Value Text
    lambdaFunctionName = CFT.mkName $ CFT.Literal prefix

    lambdaPermission :: CFT.Resource
    lambdaPermission
      = CFT.resource (prefix <> "LambdaPermission")
      $ Lambda.mkPermission "lambda:InvokeFunction" (CFT.toRef lambdaFunction) "events.amazonaws.com"
      & CFT.set @"SourceArn" (CFT.getAttArn eventsRule)

    eventsRule :: CFT.Resource
    eventsRule
      = CFT.resource logicalName
      $ Events.mkRule
      & CFT.set @"Description"        (CFT.Literal ("Trigger rule for " <> prefix))
      & CFT.set @"Name"               (CFT.mkName (CFT.Literal logicalName))
      & CFT.set @"ScheduleExpression" (CFT.Literal "rate(1 minute)")
      & CFT.set @"State"              "ENABLED"
      & CFT.set @"Targets"            [lambdaTarget]
      where
        logicalName = prefix <> "EventsRule"

        lambdaTarget =
          Events.mkTargetProperty (CFT.getAttArn lambdaFunction) (CFT.Literal logicalName)

    ingressRulePolicy
      = IAM.mkPolicyProperty
        [ ("Statement", JSON.Array [describePolicyStatement, revokePolicyStatement])
        ]
        "revoke-ingress-rules"
      where
        describePolicyStatement = JSON.Object
          [ ("Action",   JSON.Array ["ec2:DescribeSecurityGroupRules"])
          , ("Effect",   "Allow")
          , ("Resource", "*")
          ]

        revokePolicyStatement = JSON.Object
          [ ("Action",   JSON.Array ["ec2:RevokeSecurityGroupIngress"])
          , ("Effect",   "Allow")
          , ("Resource", mkResources securityGroupArnValue)
          ]

        mkResources function
          = JSON.toJSON
          $ JSON.toJSON . function . (.securityGroupIdOutput.value) <$> ingressConfigurations

        securityGroupArnValue securityGroupIdValue = CFT.Join ":"
          [ "arn"
          , "aws"
          , "ec2"
          , CFT.awsRegion
          , CFT.awsAccountId
          , CFT.Join "/" ["security-group", securityGroupIdValue]
          ]
