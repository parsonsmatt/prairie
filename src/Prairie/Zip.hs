module Prairie.Zip where

import Prairie.Class
import Prairie.Fold

-- | Take two records and zip them together with the provided function.
--
-- The field is given for the final parameter in the function, allowing you to use @LambdaCase@.
--
-- @
-- zipWithRecord
--     (\a b ->
--         \case
--             UserName ->
--                 a <> b
--             UserAge ->
--                 a + b
--     )
-- @
--
-- @since 0.0.4.0
zipWithRecord
    :: forall rec. (Record rec)
    => (forall ty. ty -> ty -> Field rec ty -> ty)
    -> rec
    -> rec
    -> rec
zipWithRecord k r0 r1 =
    foldRecord f r0 r0
  where
      f :: ty -> rec -> Field rec ty -> rec
      f v0 rec field =
          let v1 = getRecordField field r1
           in setRecordField field (k v0 v1 field) rec
