module LHT.TH (readFile) where

import Instances.TH.Lift ()
import LHT.Prelude
import Language.Haskell.TH.Syntax
import System.FilePath (FilePath)

import qualified Data.Text.IO as Text

readFile :: FilePath -> Q (TExp Text)
readFile path = do
  qAddDependentFile path
  TExp <$> (lift =<< runIO (Text.readFile path))
