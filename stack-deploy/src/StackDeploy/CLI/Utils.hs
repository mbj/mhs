module StackDeploy.CLI.Utils
  ( instanceNameOption
  , templateNameOption
  )
where

import GHC.TypeLits (KnownSymbol)
import Options.Applicative
import StackDeploy.Prelude

import qualified StackDeploy.InstanceSpec  as StackDeploy
import qualified StackDeploy.NamedTemplate as StackDeploy

instanceNameOption :: Parser StackDeploy.InstanceName
instanceNameOption =
  option
    reader
    (long "instance" <> metavar "INSTANCE" <> help "Stack instance name")

templateNameOption :: Parser StackDeploy.TemplateName
templateNameOption =
  option
    reader
    (long "template" <> metavar "TEMPLATE_NAME" <> help "Template name")

reader :: KnownSymbol a => ReadM (BoundText a)
reader = maybeReader (convertMaybe . convert @Text)
