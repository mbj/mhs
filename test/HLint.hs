import Control.Monad (unless)
import Data.List (null)
import Language.Haskell.HLint3 (hlint)
import System.Exit (exitFailure)
import System.IO (IO)

main :: IO ()
main = do
  hints <- hlint ["."]
  unless (null hints) exitFailure
