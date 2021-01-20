module StackDeploy.CLI.Utils (instanceSpecName, templateName) where

import Options.Applicative
import StackDeploy.Prelude

import qualified StackDeploy.InstanceSpec as InstanceSpec
import qualified StackDeploy.Template     as Template

instanceSpecName :: Parser (InstanceSpec.Name env)
instanceSpecName = instanceSpecNameParser (InstanceSpec.mkName <$> str)

templateName :: Parser Template.Name
templateName = templateNameParser (Template.mkName <$> str)

instanceSpecNameParser :: ReadM (InstanceSpec.Name env) -> Parser (InstanceSpec.Name env)
instanceSpecNameParser reader =
  option
    reader
    (long "instance" <> metavar "INSTANCE" <> help "Stack instance name")

templateNameParser :: ReadM Template.Name -> Parser Template.Name
templateNameParser reader =
  option
    reader
    (long "template" <> metavar "TEMPLATE" <> help "Template name")
