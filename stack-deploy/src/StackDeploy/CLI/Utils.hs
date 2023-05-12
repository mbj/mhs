module StackDeploy.CLI.Utils
  ( instanceSpecNameOption
  , templateNameArgument
  , templateNameOption
  )
where

import Options.Applicative
import StackDeploy.Prelude

import qualified StackDeploy.InstanceSpec as InstanceSpec
import qualified StackDeploy.Template     as Template

instanceSpecNameOption :: Parser (InstanceSpec.Name env)
instanceSpecNameOption = instanceSpecNameParser (InstanceSpec.mkName <$> str)

templateNameArgument :: Parser Template.Name
templateNameArgument = Template.mkName <$> argument str (metavar "TEMPLATE")

templateNameOption :: Parser Template.Name
templateNameOption = templateNameParser (Template.mkName <$> str)

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
