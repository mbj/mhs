module StackDeploy.GithubActions
  ( githubActionsAssumeRolePolicyDocument
  , githubActionsOIDCProvider
  , githubActionsOIDCProviderArnOutput
  , githubActionsOIDCProviderComponent
  )
where

import StackDeploy.Prelude

import qualified Data.Aeson                    as JSON
import qualified StackDeploy.Component         as StackDeploy
import qualified StackDeploy.Stratosphere      as CFT
import qualified Stratosphere                  as CFT
import qualified Stratosphere.IAM.OIDCProvider as IAM

githubActionsOIDCProviderComponent :: StackDeploy.Component
githubActionsOIDCProviderComponent = mempty
  { StackDeploy.resources = [githubActionsOIDCProvider]
  , StackDeploy.outputs   = [githubActionsOIDCProviderArnOutput]
  }

githubActionsOIDCProvider :: CFT.Resource
githubActionsOIDCProvider
  = CFT.resource "GithubActionsOIDCProvider"
  $ IAM.mkOIDCProvider
    -- @see https://github.blog/changelog/2023-06-27-github-actions-update-on-oidc-integration-with-aws/
    [ "6938fd4d98bab03faadb97b34396831e3780aea1"
    , "1c58a3a8518e8759bf075b76b750d4f2df264fcd"
    ]
  & CFT.set @"ClientIdList" ["sts.amazonaws.com"]
  & CFT.set @"Url"          "https://token.actions.githubusercontent.com"

githubActionsOIDCProviderArnOutput :: CFT.Output
githubActionsOIDCProviderArnOutput
  = CFT.mkResourceOutputExport githubActionsOIDCProvider "Arn" CFT.getAttArn

githubActionsAssumeRolePolicyDocument :: CFT.Value Text -> CFT.Value Text -> JSON.Object
githubActionsAssumeRolePolicyDocument stackName repository =
  [ ("Version", "2012-10-17")
  , ("Statement", JSON.Array
      [ JSON.Object
        [ ( "Action"
          , "sts:AssumeRoleWithWebIdentity"
          )
        , ( "Effect"
          , "Allow"
          )
        , ( "Principal"
          , JSON.Object
            [ ("Federated", JSON.toJSON $ CFT.mkStackImport stackName githubActionsOIDCProviderArnOutput)
            ]
          )
        , ( "Condition"
          , JSON.Object
            [ ( "StringLike"
              , JSON.Object
                [( "token.actions.githubusercontent.com:sub", JSON.toJSON repositorySub)]
              )
            ]
          )
        ]
      ]
    )
  ]
  where
    repositorySub :: CFT.Value Text
    repositorySub = CFT.Join ":" ["repo", repository, "*"]
