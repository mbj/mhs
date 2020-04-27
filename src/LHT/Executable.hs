module LHT.Executable (Executable(..)) where

import qualified Data.ByteString as BS

newtype Executable = Executable { un :: BS.ByteString }
