-- | This module provides the ability to append two records using '(<>)',
-- provided that all of their fields have an instance of 'Semigroup'.
module Prairie.Semigroup where

import Prairie.Class
import Prairie.Zip

-- | Zip two records together using a 'Semigroup' append.
--
-- @since 0.0.4.0
appendRecord
    :: forall rec. (Record rec, FieldDict Semigroup rec)
    => rec
    -> rec
    -> rec
appendRecord =
    zipWithRecord (\a b field -> withFieldDict @Semigroup field (a <> b))
