-- | This module contains a newtype 'AsRecord' which is used to provide
-- a variety of default instances for types based on their 'Record'
-- instance.
module Prairie.AsRecord where

import Prairie.Class
import Prairie.Monoid
import Prairie.Semigroup

-- | This @newtype@ is intended for use with @DerivingVia@.
--
-- For an example use, let's consider this @User@ datatype:
--
-- > data Foo = Foo
-- >     { ints :: [Int]
-- >     , char :: First Char
-- >     }
-- >
-- > mkRecord ''Foo
--
-- Let's say we want to define an instance of 'Semigroup' for this type, so
-- that we can combine two of them. Ordinarily, we'd need to write
-- a boilerplate-y instance:
--
-- > instance Semigroup Foo where
-- >     f0 <> f1 = Foo
-- >         { ints = f0.ints <> f1.ints
-- >         , char = f0.char <> f1.char
-- >         }
--
-- With @DerivingVia@, we can use 'AsRecord' to provide it easily:
--
-- @
-- deriving via AsRecord Foo instance Semigroup Foo
-- @
--
-- @since 0.0.4.0
newtype AsRecord rec = AsRecord {unAsRecord :: rec}

-- |
--
-- @since 0.0.4.0
instance (Record rec, FieldDict Semigroup rec) => Semigroup (AsRecord rec) where
    AsRecord r0 <> AsRecord r1 =
        AsRecord (appendRecord r0 r1)

-- |
--
-- @since 0.0.4.0
instance
    (Record rec, FieldDict Semigroup rec, FieldDict Monoid rec) =>
    Monoid (AsRecord rec)
    where
    mempty = AsRecord emptyRecord
