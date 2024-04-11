module Prairie.Monoid where

import Prairie.Class

-- | Create a 'mempty' 'Record' assuming that all of the fields of the
-- record have a 'Monoid' instance.
emptyRecord :: (Record rec, FieldDict Monoid rec) => rec
emptyRecord = tabulateRecord (\field -> withFieldDict @Monoid field mempty)
