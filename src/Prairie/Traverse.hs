-- | This module shows how to 'traverse' over a 'Record', allowing you to
-- perform side-effects for each field and return a transformed value.
module Prairie.Traverse where

import Data.List (foldl')
import Prairie.Class
import Prairie.Fold

-- | Apply an effectful function over each field of a 'Record', producing a new 'Record' in the process.
traverseRecord
    :: forall rec f. (Record rec, Applicative f)
    => (forall ty. Field rec ty -> ty -> f ty)
    -> rec
    -> f rec
traverseRecord f init =
    foldl' k (pure init) (recordToFieldList init)
  where
    f' :: Field rec ty -> ty -> f (SomeFieldWithValue rec)
    f' field val =
        SomeFieldWithValue field <$> f field val
    k frec (SomeFieldWithValue field val) =
        liftA2
            (\(SomeFieldWithValue field' val') rec -> setRecordField field' val' rec)
            (f' field val)
            frec
