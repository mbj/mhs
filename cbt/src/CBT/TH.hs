module CBT.TH (readDockerfileContent) where

import CBT.Prelude
import CBT.Types
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax

import qualified CBT.BuildDefinition as BuildDefinition
import qualified System.Path         as Path

readDockerfileContent :: Path.AbsRelFile -> Q (TExp DockerfileContent)
readDockerfileContent path = do
  qAddDependentFile $ Path.toString path

  TExp <$> (lift =<< runIO (BuildDefinition.readDockerfileContent path))
