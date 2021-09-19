{-# LANGUAGE CPP #-}
module CBT.TH (readDockerfileContent) where

import CBT.Prelude
import CBT.Types
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax

import qualified CBT.BuildDefinition as BuildDefinition
import qualified System.Path         as Path

#if MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
readDockerfileContent :: Path.AbsRelFile -> Code Q DockerfileContent
readDockerfileContent path = liftCode $ do
#else
readDockerfileContent :: Path.AbsRelFile -> Q (TExp DockerfileContent)
readDockerfileContent path = do
#endif
  qAddDependentFile $ Path.toString path

  TExp <$> (lift =<< runIO (BuildDefinition.readDockerfileContent path))
