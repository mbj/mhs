module CBT.Image.Name
  ( IsName(..)
  , Name(..)
  , QualifiedName(..)
  , Registry(..)
  , Repository(..)
  , SetRegistry(..)
  , SetTag(..)
  , Tag(..)
  , TaggedName(..)
  , TaglessName(..)
  , mkLocalName
  )
where

import CBT.Prelude
import Data.Kind (Type)
import Data.Maybe (catMaybes)

import qualified Data.Text as Text

newtype Registry = Registry Text
  deriving newtype (Conversion Text)
  deriving stock (Eq, Show)

newtype Repository = Repository Text
  deriving newtype (Conversion Text)
  deriving stock (Eq, Show)

newtype Tag = Tag Text
  deriving newtype (Conversion Text)
  deriving stock (Eq, Show)

data Name = Name
  { registry   :: Maybe Registry
  , repository :: Repository
  , tag        :: Maybe Tag
  }
  deriving stock (Eq, Show)

data TaglessName = TaglessName
  { registry   :: Maybe Registry
  , repository :: Repository
  }
  deriving stock (Eq, Show)

data TaggedName = TaggedName
  { registry   :: Maybe Registry
  , repository :: Repository
  , tag        :: Tag
  }
  deriving stock (Eq, Show)

data QualifiedName = QualifiedName
  { registry   :: Registry
  , repository :: Repository
  , tag        :: Tag
  }
  deriving stock (Eq, Show)

instance Conversion Text Name where
  convert Name{..}
    = Text.intercalate "/"
    $ catMaybes
    [ convert <$> registry
    , pure
       . Text.intercalate ":"
       $ catMaybes
       [ pure $ convert repository
       , convert <$> tag
       ]
    ]

instance Conversion Text TaglessName where
  convert = convert . toName

instance Conversion Text TaggedName where
  convert = convert . toName

instance Conversion Text QualifiedName where
  convert = convert . toName

class (SetTag a, SetRegistry a) => IsName a where
  toName :: a -> Name

  nameString :: a -> String
  nameString = convertVia @Text . toName

instance IsName Name where
  toName = identity

instance IsName TaglessName where
  toName TaglessName{..} = Name{tag = empty, ..}

instance IsName TaggedName where
  toName TaggedName{..} = Name{tag = pure tag, ..}

instance IsName QualifiedName where
  toName QualifiedName{..}
    = Name
    { tag = pure tag
    , registry = pure registry
    , ..
    }

class SetTag a where
  type SetTagResult a :: Type

  setTag :: a -> Tag -> SetTagResult a

instance SetTag Name where
  type SetTagResult Name = TaggedName

  setTag Name{..} newTag = TaggedName{tag = newTag, ..}

instance SetTag TaglessName where
  type SetTagResult TaglessName = TaggedName

  setTag TaglessName{..} newTag = TaggedName{tag = newTag, ..}

instance SetTag TaggedName where
  type SetTagResult TaggedName = TaggedName

  setTag TaggedName{..} newTag = TaggedName{tag = newTag, ..}

instance SetTag QualifiedName where
  type SetTagResult QualifiedName = QualifiedName

  setTag QualifiedName{..} newTag = QualifiedName{tag = newTag, ..}

class SetRegistry a where
  type SetRegistryResult a :: Type

  setRegistry :: a -> Registry -> SetRegistryResult a

instance SetRegistry Name where
  type SetRegistryResult Name = Name
  setRegistry Name{..} newRegistry = Name{registry = pure newRegistry, ..}

instance SetRegistry TaglessName where
  type SetRegistryResult TaglessName = TaglessName
  setRegistry TaglessName{..} newRegistry = TaglessName{registry = pure newRegistry, ..}

instance SetRegistry TaggedName where
  type SetRegistryResult TaggedName = QualifiedName
  setRegistry TaggedName{..} newRegistry = QualifiedName{registry = newRegistry, ..}

instance SetRegistry QualifiedName where
  type SetRegistryResult QualifiedName = QualifiedName
  setRegistry QualifiedName{..} newRegistry = QualifiedName{registry = newRegistry, ..}

mkLocalName :: Text -> TaglessName
mkLocalName name
  = TaglessName
  { registry   = empty
  , repository = Repository name
  }
