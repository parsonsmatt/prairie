-- | This module shows you how to fold a record.
module Prairie.Fold where

import Control.Monad (foldM)
import Prairie.Class
import Data.List (foldl')

-- | A datatype containing a 'Field' along with a value for that field.
data SomeFieldWithValue rec where
    SomeFieldWithValue :: Field rec a -> a -> SomeFieldWithValue rec

-- | Convert a 'Record' into a list of the records fields paired with the
-- value for that record.
recordToFieldList :: forall rec. (Record rec) => rec -> [SomeFieldWithValue rec]
recordToFieldList rec =
    fmap
        (\(SomeField field) ->
            SomeFieldWithValue field (getRecordField field rec)
        )
        (allFields @rec)

-- | Fold over the fields of a record to produce a final result.
--
-- The function parameter accepts the @'Field' rec ty@, a value in the
-- record, and the accumulator. Example:
--
-- @
-- foldRecord
--      (\\ field val acc ->
--          case field of
--              UserName ->
--                  length val + acc
--              UserAge ->
--                  val + acc
--      )
--      0
--      User { userName = \"Matt\", userAge = 35 }
-- @
foldRecord
    :: forall rec r
     . (Record rec)
    => (forall ty. Field rec ty -> ty -> r -> r)
    -> r
    -> rec
    -> r
foldRecord k init rec =
    foldl'
        (\acc (SomeFieldWithValue field value) ->
            k field value acc
        )
        init
        (recordToFieldList rec)

-- | Fold over a 'Record' with a monadic action.
foldMRecord
    :: forall rec m r
     . (Record rec, Monad m)
    => (forall ty. Field rec ty -> ty -> r -> m r)
    -> r
    -> rec
    -> m r
foldMRecord k init rec =
    foldM
        (\acc (SomeFieldWithValue field value) ->
            k field value acc
        )
        init
        (recordToFieldList rec)

-- | Convert each field of a 'Record' into a monoidal value and combine
-- them together using 'mappend'.
foldMapRecord
    :: forall rec m
    . (Record rec, Monoid m)
    => (forall ty. Field rec ty -> ty -> m)
    -> rec
    -> m
foldMapRecord k rec =
    foldMap (\(SomeFieldWithValue f v) -> k f v) (recordToFieldList rec)
