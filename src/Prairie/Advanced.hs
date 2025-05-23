{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
module Prairie.Advanced
    where

import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Text (Text)
import Prairie.Class (Field, getRecordField, recordFieldLabel, tabulateRecordA)
import qualified Prairie.Class as Simple
import Prelude hiding (sequenceA, zipWith)

newtype Record f rec = Record
    { getField :: forall x. Field rec x -> f x }

toAdvanced :: Simple.Record rec => rec -> Record Identity rec
toAdvanced rec =
    Record $ \field -> Identity $ getRecordField field rec

fromAdvanced :: Simple.Record rec => Record Identity rec -> rec
fromAdvanced = runIdentity . sequenceA

distribute :: (Functor f, Simple.Record rec) => f rec -> Record f rec
distribute fRec = Record $ \field ->
    fmap (getRecordField field) fRec

sequenceA :: (Applicative f, Simple.Record rec) => Record f rec -> f rec
sequenceA (Record fn) = tabulateRecordA fn

zipWith :: (forall x. f x -> g x -> h x) -> Record f rec -> Record g rec -> Record h rec
zipWith zipFn (Record lhs) (Record rhs) = Record $ \field ->
    zipFn (lhs field) (rhs field)

fieldNames :: Simple.Record rec => Record (Const Text) rec
fieldNames = buildConst recordFieldLabel

toList :: Simple.Record rec => Record (Const a) rec -> [(Text, a)]
toList rec =
    getConst $
    sequenceA $
    zipWith
        (\(Const fieldName) (Const a) -> Const [(fieldName, a)])
        fieldNames
        rec

buildConst :: forall a rec. (forall x. Field rec x -> a) -> Record (Const a) rec
buildConst getValue =
    Record $ \field ->
        Const $ getValue field
