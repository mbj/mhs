module CBT.Image.Name
  ( Name(..)
  , QualifiedName(..)
  , Registry(..)
  , Repository(..)
  , Tag(..)
  , TaggedName(..)
  , mkLocalName
  , setRegistry
  , setTag
  )
where

import CBT.Prelude
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

data TaggedName = TaggedName
  { registry   :: Maybe Registry
  , repository :: Repository
  , tag        :: Tag
  }

instance Conversion Text TaggedName where
  convert TaggedName{..}
    = Text.intercalate "/"
    $ catMaybes
    [ convert <$> registry
    , pure
       $ Text.intercalate ":"
       [ convert repository
       , convert tag
       ]
    ]

data QualifiedName = QualifiedName
  { registry   :: Registry
  , repository :: Repository
  , tag        :: Tag
  }

instance Conversion Text QualifiedName where
  convert QualifiedName{..}
    = convert registry
    <> "/" <> convert repository
    <> ":" <> convert tag

setTag :: Name -> Tag -> TaggedName
setTag Name{..} newTag
  = TaggedName
  { tag = newTag
  , ..
  }

setRegistry :: TaggedName -> Registry -> QualifiedName
setRegistry TaggedName{..} newRegistry
  = QualifiedName
  { registry = newRegistry
  , ..
  }


mkLocalName :: Text -> Name
mkLocalName name
  = Name
  { registry   = empty
  , repository = Repository name
  , tag        = empty
  }
