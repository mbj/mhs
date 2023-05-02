module PGT.Output.Render where

import PGT.Prelude

class Render a where
  render :: a -> Text

instance Render Text where
  render = identity
