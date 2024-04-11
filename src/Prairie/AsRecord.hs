-- | This module contains a newtype 'AsRecord' which is used to provide
-- a variety of default instances for types based on their 'Record'
-- instance.
module Prairie.AsRecord where

import Prairie.Class

newtype AsRecord rec = AsRecord { unAsRecord :: rec }

instance (Record rec, FieldDict Semigroup rec) => Semigroup (AsRecord rec) where
    AsRecord r0 <> AsRecord r1 =
        fold
