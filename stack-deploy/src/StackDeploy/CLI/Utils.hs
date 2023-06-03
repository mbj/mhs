module StackDeploy.CLI.Utils
  ( instanceSpecNameOption
  , templateNameOption
  )
where

import GHC.TypeLits (KnownSymbol)
import Options.Applicative
import StackDeploy.Prelude

import qualified StackDeploy.InstanceSpec as InstanceSpec
import qualified StackDeploy.Template     as Template

instanceSpecNameOption :: Parser InstanceSpec.Name
instanceSpecNameOption =
  option
    reader
    (long "instance" <> metavar "INSTANCE" <> help "Stack instance name")

templateNameOption :: Parser Template.Name
templateNameOption =
  option
    reader
    (long "template" <> metavar "TEMPLATE" <> help "Template name")

reader :: KnownSymbol a => ReadM (BoundText a)
reader = maybeReader (convertMaybe . convert @Text)
