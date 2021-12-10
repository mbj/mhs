module XRay.Parser (fixedHex) where

import Data.Attoparsec.Text (Parser, take)
import Data.Bits (Bits, (.|.), shiftL)
import Data.Char (Char, ord)
import Data.Foldable (foldlM)
import Data.Word (Word8)
import GHC.Num ((-))
import GHC.Real (fromIntegral)
import Prelude (Integral)
import XRay.Prelude

-- | Parser for fixed width hexedecimal representations
--
-- $setup
-- >>> import Control.Applicative (Applicative((<*)))
-- >>> import Data.Attoparsec.Text (endOfInput, parseOnly)

-- | Parse fixed hexadecimal number
--
-- Examples:
--
-- >>> let parser = (fixedHex 4) <* endOfInput
-- >>> parseOnly parser "0000"
-- Right 0
-- >>> parseOnly parser "00FF"
-- Right 255
-- >>> parseOnly parser "00ff"
-- Right 255
-- >>> parseOnly parser "fff"  -- too short
-- Left "not enough input"
-- >>> parseOnly parser "fffff"  -- too long
-- Left "endOfInput"
-- >>> parseOnly parser ""  -- empty
-- Left "not enough input"
-- >>> parseOnly parser "xxxx"  -- not hex
-- Left "Failed reading: fixedHex"
fixedHex :: forall a . (Bits a, Integral a) => Word8 -> Parser a
fixedHex length = do
  (chars :: String) <- convertText <$> take (fromIntegral length)

  maybe (fail "fixedHex") pure $ foldlM step 0 chars

  where
    step :: a -> Char -> Maybe a
    step acc char | within '0' '9' = pure $ from '0' 0
                  | within 'a' 'f' = pure $ from 'a' 10
                  | within 'A' 'F' = pure $ from 'A' 10
                  | otherwise      = empty
      where
        from :: Char -> a -> a
        from left' base = (acc `shiftL` 4) .|. fromIntegral (ord char - ord left') + base

        within left' right = left' <= char && char <= right
