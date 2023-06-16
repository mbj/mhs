module Test.Data.Conversions.Integral (testTree) where

import Data.Conversions
import Data.Int (Int, Int16, Int32, Int64, Int8)
import Data.Scientific (Scientific)
import Data.Tuple (fst)
import Data.Typeable (Typeable)
import Data.Word (Word, Word16, Word32, Word64, Word8)
import GHC.Enum (pred, succ)
import GHC.Integer (Integer)
import GHC.Real (properFraction)
import MPrelude

import qualified Data.Conversions.Hedgehog as Test
import qualified Hedgehog
import qualified Hedgehog.Gen
import qualified Hedgehog.Range
import qualified Test.Tasty                as Tasty

type BoundResult a b = Either (BoundError a b) b
type Result a b      = Either (UserBoundError a b) b

testTree :: Tasty.TestTree
testTree
  = Tasty.testGroup "Integral"
    [ intToWord
    , integerToIntegral
    , integersToNatural
    , integerToScientific
    , naturalToWord
    , word64ToInt
    ]

intToWord :: Tasty.TestTree
intToWord
  = Tasty.testGroup "Int to Word conversions"
    [ toWord @Word8
    , toWord @Word16
    , toWord @Word32
    , toWord64
    ]
  where
    toWord
      :: forall word
      .  ( Conversion Int word
         , Conversion (BoundResult Int word) Int
         , Bounded word
         , Show word
         , Typeable word
         )
      => Tasty.TestTree
    toWord = Test.run biConvert generators
      where
        generators :: Test.Generators Int
        generators = Test.Generators
          { success = [Hedgehog.Gen.int range]
          , failure =
              [ pure -1
              , pure $ maxBound @Int
              ]
          }
          where
            range :: Hedgehog.Range Int
            range = Hedgehog.Range.constant 0 (convert $ maxBound @word)

        biConvert :: Test.BiConvert word Int
        biConvert = Test.BiConvert
          { toTarget   = Test.mkConvertEither $ convert @(BoundResult Int word)
          , fromTarget = pure . convert
          }

    toWord64 :: Tasty.TestTree
    toWord64 = Test.run biConvert generators
      where
        generators :: Test.Generators Int
        generators = Test.Generators
          { success = [Hedgehog.Gen.int range]
          , failure = [pure -1]
          }
          where
            range :: Hedgehog.Range Int
            range = Hedgehog.Range.constant 0 maxBound

        biConvert :: Test.BiConvert Word64 Int
        biConvert = Test.BiConvert
          { toTarget   = Test.mkConvertEither $ convert @(BoundResult Int Word64)
          , fromTarget = Test.mkConvertEither $ convert @(BoundResult Word64 Int)
          }

integerToIntegral :: Tasty.TestTree
integerToIntegral
  = Tasty.testGroup "Integer to Integral conversions"
    [ toInteger @Int
    , toInteger @Int8
    , toInteger @Int16
    , toInteger @Int32
    , toInteger @Int64
    , toInteger @Word
    , toInteger @Word8
    , toInteger @Word16
    , toInteger @Word32
    , toInteger @Word64
    ]
  where
    toInteger
      :: forall integral
      .  ( Conversion Integer integral
         , Conversion (BoundResult Integer integral) Integer
         , Bounded integral
         , Show integral
         , Typeable integral
         )
      => Tasty.TestTree
    toInteger = Test.run biConvert generators
      where
        generators :: Test.Generators Integer
        generators = Test.Generators
          { success = [Hedgehog.Gen.integral range]
          , failure =
              [ pure . succ . convert $ maxBound @Word64
              , pure . pred . convert $ minBound @Int64
              ]
          }
          where
            range :: Hedgehog.Range Integer
            range = convert <$> Hedgehog.Range.constant @integral minBound maxBound

        biConvert :: Test.BiConvert integral Integer
        biConvert = Test.BiConvert
          { toTarget   = Test.mkConvertEither $ convert @(BoundResult Integer integral)
          , fromTarget = pure . convert
          }

integersToNatural :: Tasty.TestTree
integersToNatural
  = Tasty.testGroup "Integers to Natural conversions"
    [ toNatural Hedgehog.Gen.int
    , toNatural Hedgehog.Gen.int8
    , toNatural Hedgehog.Gen.int16
    , toNatural Hedgehog.Gen.int32
    , toNatural Hedgehog.Gen.int64
    , integerToNatural
    ]
  where
    toNatural
      :: forall integer
      .  ( Bounded integer
         , Conversion (Result Natural integer) Natural
         , Conversion (Result integer Natural) integer
         , Num integer
         , Show integer
         , Typeable integer
         )
      => (Hedgehog.Range integer -> Hedgehog.Gen integer)
      -> Tasty.TestTree
    toNatural genInteger
      = Test.run biConvert generators
      where
        generators :: Test.Generators integer
        generators = Test.Generators
          { success = [genInteger range]
          , failure = [pure -1]
          }
          where
            range :: Hedgehog.Range integer
            range = Hedgehog.Range.constant 0 maxBound

        biConvert :: Test.BiConvert Natural integer
        biConvert = Test.BiConvert
          { toTarget   = Test.mkConvertEither $ convert @(Result integer Natural)
          , fromTarget = Test.mkConvertEither $ convert @(Result Natural integer)
          }

    integerToNatural :: Tasty.TestTree
    integerToNatural
      = Test.run biConvert generators
      where
        biConvert :: Test.BiConvert Natural Integer
        biConvert = Test.BiConvert
          { toTarget   = Test.mkConvertEither $ convert @(Either (UserBoundError Integer Text) Natural)
          , fromTarget = pure . convert
          }

        generators :: Test.Generators Integer
        generators = Test.Generators
          { success = [Hedgehog.Gen.integral range]
          , failure = [pure -1]
          }
          where
            range :: Hedgehog.Range Integer
            range = Hedgehog.Range.constant 0 (succ . convert $ maxBound @Int64)

integerToScientific :: Tasty.TestTree
integerToScientific = Test.run biConvert generators
  where
    generators :: Test.Generators Integer
    generators = Test.Generators
      { success = [convert <$> Hedgehog.Gen.int64 range]
      , failure = []
      }
      where
        range :: Hedgehog.Range Int64
        range = Hedgehog.Range.constant minBound maxBound

    biConvert :: Test.BiConvert Scientific Integer
    biConvert = Test.BiConvert
      { toTarget   = pure . convert
      , fromTarget = pure . (fst . properFraction)
      }

naturalToWord :: Tasty.TestTree
naturalToWord
  = Tasty.testGroup "Natural to Word conversions"
    [ toWord Hedgehog.Gen.word
    , toWord Hedgehog.Gen.word8
    , toWord Hedgehog.Gen.word16
    , toWord Hedgehog.Gen.word32
    , toWord Hedgehog.Gen.word64
    ]
  where
    toWord
      :: forall word
      .  ( Bounded word
         , Conversion Natural word
         , Conversion (Result Natural word) Natural
         , Show word
         , Typeable word
         )
      => (Hedgehog.Range word -> Hedgehog.Gen word)
      -> Tasty.TestTree
    toWord genSource
      = Test.run biConvert generators
      where
        generators :: Test.Generators Natural
        generators = Test.Generators
          { success = [convert <$> genSource range]
          , failure = [pure . succ . convert $ maxBound @Word64]
          }

        range :: Hedgehog.Range word
        range = Hedgehog.Range.constant minBound maxBound

        biConvert :: Test.BiConvert word Natural
        biConvert = Test.BiConvert
          { toTarget   = Test.mkConvertEither $ convert @(Result Natural word)
          , fromTarget = pure . (convert @Natural @word)
          }

word64ToInt :: Tasty.TestTree
word64ToInt = Test.run biConvert generators
  where
    generators :: Test.Generators Word64
    generators = Test.Generators
      { success = [Hedgehog.Gen.word64 range]
      , failure = [pure maxBound]
      }
      where
        range :: Hedgehog.Range Word64
        range = Hedgehog.Range.constant minBound (convertImpure $ maxBound @Int)

    biConvert :: Test.BiConvert Int Word64
    biConvert = Test.BiConvert
      { toTarget   = Test.mkConvertEither $ convert @(BoundResult Word64 Int)
      , fromTarget = Test.mkConvertEither $ convert @(BoundResult Int Word64)
      }
