module PGT.Formatter (multilineFailedFormatter) where

import Control.Monad ((>>), (>>=), unless)
import Data.Char (Char)
import Data.String (String)
import Data.Tuple (uncurry)
import Numeric.Natural (Natural)
import PGT.Prelude
import Test.Hspec.Core.Util (formatRequirement, joinPath)
import Text.Read (read)

import qualified Data.Algorithm.Diff   as Diff
import qualified Data.Foldable         as Foldable
import qualified Data.List             as List
import qualified Data.String           as String
import qualified Data.Text             as Text
import qualified Test.Hspec.Formatters as Format

multilineFailedFormatter :: Format.FormatM ()
multilineFailedFormatter = do
  failures <- Format.getFailMessages
  unless (Foldable.null failures) $ do
    Format.writeLine "Failures:"
    Format.writeLine ""
    Foldable.mapM_ (uncurry formatFailure) (List.zip [1..] failures)
    Format.write "Randomized with seed " >> Format.usedSeed >>= Format.writeLine . show
    Format.writeLine ""
  where
    formatFailure :: Natural -> Format.FailureRecord -> Format.FormatM ()
    formatFailure index (Format.FailureRecord _location path reason) = do
      Format.write $ show index <> ") "
      Format.writeLine $ formatRequirement path
      formatReason reason
      Format.writeLine ""
      Format.writeLine ("  To rerun use: --match " <> show (joinPath path))
      Format.writeLine ""

    formatReason (Format.Error _location exception)
      = Format.withFailColor
      . indent
      $ (("uncaught exception: " <>) . Format.formatException) exception

    formatReason (Format.ExpectedButGot _preface expected actual) = do
      Format.withFailColor $ Format.writeLine "unexpected diff: "
      Foldable.mapM_ writeChunk $ diff (read expected) (read actual)

    formatReason (Format.Reason err) = Format.withFailColor $ indent err
    formatReason Format.NoReason     = pure ()

    indent = Foldable.mapM_ Format.writeLine . String.lines

    writeChunk (Diff.Both   lines _) = writeLines Format.write        ' ' lines
    writeChunk (Diff.First  lines  ) = writeLines Format.extraChunk   '+' lines
    writeChunk (Diff.Second lines  ) = writeLines Format.missingChunk '-' lines

    writeLines
      :: (String -> Format.FormatM ())
      -> Char
      -> [Text]
      -> Format.FormatM ()
    writeLines action symbol = Foldable.mapM_ $ \line -> do
      action $ symbol : convertText line
      Format.writeLine ""

diff :: Text -> Text -> [Diff.Diff [Text]]
diff expected actual = Diff.getGroupedDiff (Text.lines expected) (Text.lines actual)
