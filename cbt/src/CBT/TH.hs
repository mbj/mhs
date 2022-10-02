module CBT.TH (readDockerfileContent) where

import CBT.Prelude
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax

import qualified CBT.Image.BuildDefinition as CBT.Image
import qualified System.Path               as Path

readDockerfileContent :: Path.AbsRelFile -> Code Q CBT.Image.DockerfileContent
readDockerfileContent path = liftCode $ do
  qAddDependentFile $ Path.toString path

  TExp <$> (lift =<< runIO (CBT.Image.readDockerfileContent path))
