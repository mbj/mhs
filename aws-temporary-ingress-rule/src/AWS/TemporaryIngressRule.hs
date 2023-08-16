module AWS.TemporaryIngressRule
  ( Config(..)
  , Env
  , WithTemporaryIngressRule
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
import qualified Amazonka
import qualified Amazonka.CloudFormation.Types              as CloudFormation
import qualified Amazonka.EC2.AuthorizeSecurityGroupIngress as EC2
import qualified Amazonka.EC2.CreateTags                    as EC2
import qualified Amazonka.EC2.DescribeSecurityGroupRules    as EC2
import qualified Amazonka.EC2.RevokeSecurityGroupIngress    as EC2
import qualified Amazonka.EC2.Types                         as EC2
import qualified Data.Conduit                               as Conduit
import qualified Data.Conduit.Combinators                   as Conduit
import qualified Data.Foldable                              as Foldable
import qualified Data.Text.IO                               as IO
import qualified Data.Time                                  as Time
import qualified Data.Time.Format.ISO8601                   as Time
import qualified MIO.Amazonka                               as AWS
import qualified MIO.Log                                    as Log
import qualified Network.HTTP.MClient                       as HTTP
import qualified Network.IP.Addr                            as Network
import qualified Options.Applicative                        as CLI
import qualified StackDeploy.CLI.Utils                      as StackDeploy.CLI
import qualified StackDeploy.InstanceSpec
import qualified StackDeploy.Stack                          as StackDeploy
import qualified StackDeploy.Utils
import qualified Stratosphere                               as CFT
import qualified System.Exit                                as System
import qualified UnliftIO.Exception                         as UnliftIO

type Env env = (AWS.Env env, HTTP.Env env, Log.Env env)

type NetAddr = Network.NetAddr Network.IP

data Config = Config
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

withTemporaryIngressRule :: Env env => Config -> WithTemporaryIngressRule env a
withTemporaryIngressRule config stack action = authorizeIngressRule config stack >> action

authorizeIngressRule :: forall env . Env env => Config -> CloudFormation.Stack -> MIO env ()
authorizeIngressRule Config{..} stack = do
  request <- readRequest
  maybe (createIngressRule request) (updateIngressRuleLastSeen request) =<< findIngressRule request
  where
    readRequest :: MIO env Request
    readRequest = do
      groupId <- liftIO $ StackDeploy.Utils.fetchOutput stack securityGroupIdOutput
      netAddr <- AWS.Checkip.readNetAddr
      now     <- liftIO Time.getCurrentTime
      port    <- read . convert <$> liftIO (StackDeploy.Utils.fetchOutput stack portOutput)
      pure Request{cidr = convert @Text @String $ Network.printNetAddr netAddr, ..}

revokeExpiredIngressRules :: forall env . Env env => [Config] -> CloudFormation.Stack -> MIO env ()
revokeExpiredIngressRules config stack =
  Conduit.runConduit $ expiredIngressRulesC config stack .| Conduit.mapM_ revoke
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

listExpiredIngressRules :: forall env . Env env => [Config] -> CloudFormation.Stack -> MIO env ()
listExpiredIngressRules config stack = do
  Conduit.runConduit
    $  expiredIngressRulesC config stack
    .| Conduit.mapM_ (liftIO . IO.putStrLn . formatSecurityGroupRule)

formatSecurityGroupRule :: EC2.SecurityGroupRule -> Text
formatSecurityGroupRule rule
  =  field "cidr" (.cidrIpv4)
  <> field "cidr" (.cidrIpv6)
  <> field "Port" (fmap (convert . show) . (.fromPort))
  where
    field :: Text -> (EC2.SecurityGroupRule -> Maybe Text) -> Text
    field label access = maybe "" (\value -> label <> ": " <> value <> " ") (access rule)

data TemporaryIngressRule = TemporaryIngressRule
  { lastSeenAt        :: Time.UTCTime
  , securityGroupRule :: EC2.SecurityGroupRule
  }

temporaryIngressRulesC
  :: forall env . Env env
  => [Config]
  -> CloudFormation.Stack
  -> ConduitT () TemporaryIngressRule (MIO env) ()
temporaryIngressRulesC config stack = do
  groupIds <- traverse
     (liftIO . StackDeploy.Utils.fetchOutput stack . (.securityGroupIdOutput)) config

  AWS.paginate (mkRequest groupIds)
    .| Conduit.concatMap (fromMaybe [] . (.securityGroupRules))
    .| Conduit.mapM parseRule
  where
    mkRequest groupIds
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
  => [Config]
  -> CloudFormation.Stack
  -> ConduitT () EC2.SecurityGroupRule (MIO env) ()
expiredIngressRulesC config stack = do
   now <- liftIO Time.getCurrentTime

   temporaryIngressRulesC config stack
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


parserInfo :: forall env . Env env => [Config] -> CLI.ParserInfo (MIO env System.ExitCode)
parserInfo configs = CLI.info (CLI.helper <*> subcommands) CLI.idm
  where
    subcommands
      =  CLI.hsubparser
      $  mkCommand
           "config"
           (pure $ printConfigurations $> System.ExitSuccess)
           "show configuration"
      <> mkCommand
           "authorize-all"
           (runStack authorizeAll <$> StackDeploy.CLI.instanceSpecNameOption)
           "authorize all ingress configurations"
      <> mkCommand
           "revoke-expired"
           (runStack (revokeExpiredIngressRules configs) <$> StackDeploy.CLI.instanceSpecNameOption)
           "revoke expired ingress rules"
      <> mkCommand
           "list-expired"
           (runStack (listExpiredIngressRules configs) <$> StackDeploy.CLI.instanceSpecNameOption)
           "list expired ingress rules"

    printConfigurations  = traverse_ (liftIO . IO.putStrLn . convert . show) configs

    runStack
      :: (CloudFormation.Stack -> MIO env ())
      -> StackDeploy.InstanceSpec.Name
      -> MIO env System.ExitCode
    runStack action instanceSpecName = do
      stack <- StackDeploy.getExistingStack instanceSpecName
      action stack
      pure System.ExitSuccess

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
