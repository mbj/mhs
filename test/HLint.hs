import Control.Monad (unless)
import Data.Foldable (Foldable(null))
import Language.Haskell.HLint3 (hlint)
import System.Exit (exitFailure)
import System.IO (IO)

main :: IO ()
main = do
  hints <- hlint ["src", "test"]
  unless (null hints) exitFailure
