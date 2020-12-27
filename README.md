# Bounded

User definable Integral and Text bounded types that are constrained by their set bound range with
compile time correctness guarantees.

## Examples usage

```haskell
import Data.Bounded
import Data.Text
import GHC.Natural
import Prelude

type Year = BoundNumber "Years" '(2000, 2020)

-- | Type safe construction of a recent year via type-level literals
-- providing a value greater than 2010 should result into a compiler error
recentYear :: Year
recentYear = fromType @2000

possiblyWrongYearFromDB :: Natural
possiblyWrongYearFromDB = 2030

maybeRecentYear :: Maybe Year
maybeRecentYear = convertMaybe possiblyWrongYearFromDB

eitherRecentYear :: Either (UserBoundError Natural Year) Year
eitherRecentYear = convert possiblyWrongYearFromDB

-- | Unsafe construction of a year; providing a value greater than
-- 2020 should result into a runtime error
unsafeRecentYear :: Year
unsafeRecentYear = convertUnsafe possiblyWrongYearFromDB

recentYearNumber :: Natural
recentYearNumber = convert recentYear

type FirstName = BoundText' "First Name" '(2, 10)

-- | Type safe construction with compile time compile time correctness guarantees
-- providing a single character or name greater than 10 characters should result into
-- a compile time error.
firstName :: FirstName
firstName = fromType @"Allan"

-- | Bi-direction conversion example
firstNameText :: Text
firstNameText = convert firstName

firstName' :: FirstName
firstName' = convertUnsafe firstNameText

-- | Using Bounded text with phantomTypes

data Retailer
data OnlineCommerce

type ExternalId a = BoundText' ("External Id" ++: a) '(1, 50)

type RetailerId = ExternalId Retailer
type OnlineCommerceId = ExternalId OnlineCommerce

-- See tests for more examples.
```

### Documentation

This README is tested by `markdown-unlit` to make sure the code builds. To keep _that_ happy, we do need a `main` in this file, so ignore the following :)

```haskell
main :: IO ()
main = pure ()
```
