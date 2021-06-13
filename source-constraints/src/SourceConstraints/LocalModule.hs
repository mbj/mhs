module SourceConstraints.LocalModule
  ( LocalModule(..)
  , isLocalModule
  , localModuleParser
  )
where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Bool
import Data.Char
import Data.Eq
import Data.Function
import Data.Semigroup
import Module
import qualified Data.List as List

newtype LocalModule = LocalModule ModuleName

localModuleParser :: Parser LocalModule
localModuleParser = LocalModule <$> (string "local:" *> moduleNameParser)

moduleNameParser :: Parser ModuleName
moduleNameParser = do
  first  <- section
  others <- many (char '.' *> section)

  pure . mkModuleName $ first <> List.intercalate "." others
  where
    section = (:) <$> satisfy isUpper <*> many (satisfy isLower)

isLocalModule :: LocalModule -> ModuleName -> Bool
isLocalModule (LocalModule localModuleName) moduleName =
  localModuleName == moduleName || prefixMatch
    where
      prefixMatch =
        (moduleNameString localModuleName <> ".")
        `List.isPrefixOf`
        moduleNameString moduleName
