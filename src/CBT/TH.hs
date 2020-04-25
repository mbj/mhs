module CBT.TH (readDockerfile) where

import CBT.Prelude
import CBT.Types
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax

import qualified CBT.BuildDefinition as BuildDefinition
import qualified System.Path         as Path

readDockerfile :: Prefix -> Path.AbsRelFile -> Q (TExp BuildDefinition)
readDockerfile prefix path = do
  qAddDependentFile $ Path.toString path

  TExp <$> (lift =<< runIO (BuildDefinition.readDockerfile prefix path))
