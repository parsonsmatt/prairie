-- | This module contains a utility for diffing two records.
--
-- @since 0.0.1.0
module Prairie.Diff
  ( module Prairie.Diff
  , module Prairie.Update
  ) where

import Prairie.Update
import Prairie.Class

-- | Given two 'Record's, this function produces a list of 'Update's that
-- can be performed on the first record such that it will equal the second.
--
-- @
-- 'updateRecord' ('diffRecord' old new) old == new
-- @
--
-- A @['Update' rec]@ can be serialized with 'ToJSON', sent over the wire, and
-- parsed with 'FromJSON', so you can efficiently and easily represent
-- patches to 'Record's.
--
-- @since 0.0.1.0
diffRecord
  :: (Record rec, FieldDict Eq rec)
  => rec
  -- ^ The old record.
  -> rec
  -- ^ The new record.
  -> [Update rec]
diffRecord old new = foldMap go allFields
  where
    go (SomeField f) =
      withFieldDict @Eq f $
        let
          newVal = getRecordField f new
         in
          if getRecordField f old /= newVal
            then [SetField f newVal]
            else []

