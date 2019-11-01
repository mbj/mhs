module DBT.TH (readFile) where

import DBT.Prelude
import Data.ByteString (ByteString)
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax

import qualified Data.ByteString as ByteString
import qualified System.Path     as Path

readFile :: Path.AbsRelFile -> Q (TExp ByteString)
readFile path = do
  qAddDependentFile filepath
  TExp <$> (lift =<< runIO (ByteString.readFile filepath))
  where
    filepath = Path.toString path
