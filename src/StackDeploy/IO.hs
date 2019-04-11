module StackDeploy.IO (say) where

import Data.Text.IO (putStrLn)
import StackDeploy.Prelude

say :: (MonadIO m, ToText a) => a -> m ()
say = liftIO . putStrLn . toText
