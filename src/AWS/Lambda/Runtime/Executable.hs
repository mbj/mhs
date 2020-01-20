module AWS.Lambda.Runtime.Executable
  ( Executable(..)
  )
where

import Data.ByteString (ByteString)

newtype Executable = Executable ByteString
