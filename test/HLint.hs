import Control.Monad (unless)
import Language.Haskell.HLint3 (hlint)
import System.Exit (exitFailure)
import System.IO (IO)

import qualified Data.Foldable as Foldable

main :: IO ()
main = do
  hints <- hlint ["src", "test"]
  unless (Foldable.null hints) exitFailure
