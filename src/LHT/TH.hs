module LHT.TH (readFile) where

import Instances.TH.Lift ()
import LHT.Prelude
import Language.Haskell.TH.Syntax

import qualified Data.Text.IO as Text
import qualified System.Path  as Path

readFile :: Path.AbsRelFile -> Q (TExp Text)
readFile path = do
  qAddDependentFile filepath
  TExp <$> (lift =<< runIO (Text.readFile filepath))
  where
    filepath = Path.toString path
