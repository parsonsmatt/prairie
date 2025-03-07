-- | This module shows you how to fold a 'Record'.
--
-- These utilities are based on 'recordToFieldList', which converts
-- a 'Record' into a 'SomeFieldWithValue'. Then, 'foldRecord' unpacks that
-- GADT for you, which allows you to know the type of the field and combine
-- them.
--
-- @since 0.0.3.0
module Prairie.Fold where

import Control.Monad (foldM)
import Data.List (foldl')
import Prairie.Class

-- | A datatype containing a 'Field' along with a value for that field.
--
-- @since 0.0.3.0
data SomeFieldWithValue rec where
    SomeFieldWithValue :: Field rec a -> a -> SomeFieldWithValue rec

-- | Convert a 'Record' into a list of the records fields paired with the
-- value for that record.
--
-- @since 0.0.3.0
recordToFieldList :: forall rec. (Record rec) => rec -> [SomeFieldWithValue rec]
recordToFieldList rec =
    fmap
        ( \(SomeField field) ->
            SomeFieldWithValue field (getRecordField field rec)
        )
        (allFields @rec)

-- | Fold over the fields of a record to produce a final result.
--
-- The function parameter accepts the @'Field' rec ty@, a value in the
-- record, and the accumulator. Example:
--
-- @
-- 'foldRecord'
--      (\\ val acc field ->
--          case field of
--              UserName ->
--                  length val + acc
--              UserAge ->
--                  val + acc
--      )
--      0
--      User { userName = \"Matt\", userAge = 35 }
-- @
--
-- The paramater list is given to enable 'LambdaCase' nicety:
--
-- @
-- 'foldRecord'
--      (\\val acc -> \\case
--          UserName ->
--              length val + acc
--          UserAge ->
--              val + acc
--      )
-- @
--
-- @since 0.0.3.0
foldRecord
    :: forall rec r
     . (Record rec)
    => (forall ty. ty -> r -> Field rec ty -> r)
    -> r
    -> rec
    -> r
foldRecord k init rec =
    foldl'
        ( \acc (SomeFieldWithValue field value) ->
            k value acc field
        )
        init
        (recordToFieldList rec)

-- | Fold over a 'Record' with a monadic action.
--
-- @since 0.0.3.0
foldMRecord
    :: forall rec m r
     . (Record rec, Monad m)
    => (forall ty. ty -> r -> Field rec ty -> m r)
    -> r
    -> rec
    -> m r
foldMRecord k init rec =
    foldM
        ( \acc (SomeFieldWithValue field value) ->
            k value acc field
        )
        init
        (recordToFieldList rec)

-- | Convert each field of a 'Record' into a monoidal value and combine
-- them together using 'mappend'.
--
-- @since 0.0.3.0
foldMapRecord
    :: forall rec m
     . (Record rec, Monoid m)
    => (forall ty. ty -> Field rec ty -> m)
    -> rec
    -> m
foldMapRecord k rec =
    foldMap (\(SomeFieldWithValue f v) -> k v f) (recordToFieldList rec)
