-- |  This module provides an advanced functionality for working with
-- "Prairie" 'Record's. The 'Distributed' type wraps each field in a type
-- constructor, allowing you to work flexibly with 'Record's that are
-- construted and manipulated effectfully.
--
-- As an example, consider a @'Distributed' Parser rec@. This would be
-- similar to a @'Parser' rec@, but instead of producing a complete @rec@,
-- we actually have a field-wise 'Parser' - this allows us to access the
-- result of parsing a single field without requiring that all fields parse
-- correctly.
--
-- For another example, consider a @'Distributed' DB rec@. This would
-- describe a means of producing a @rec@ by describing how to produce each
-- 'Field' of that @rec@ using the @DB@ type - possibly by fetching the
-- appropriate row.
--
-- This module provides power equivalent to the @HKD@ design pattern, but
-- without requiring any complication to the underlying datatypes.
--
-- @since 0.1.0.0
module Prairie.Distributed where

import Data.Functor.Apply (Apply (..))
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import GHC.Records
import Prairie.Class

-- | A @'Distributed' f rec@ is a 'Record' with fields that are wrapped in
-- the @f@ type constructor.
--
-- The simplest example is the trivial 'Identity' - @'Distributed'
-- 'Identity' rec@ wraps each field in 'Identity', which doesn't do
-- anything.
--
-- But you can also represent a @'Distributed' 'IO' rec@, where each field
-- is an 'IO' action.
--
-- This type is equivalently powerful to the @Higher Kinded Data@ pattern,
-- but significantly more flexible, since you don't need to munge the
-- underlying datatype with the complexity of this.
--
-- You can use @OverloadedRecordDot@ to access fields on this directly, or
-- you can also use 'getRecordFieldDistributed'.
--
-- @since 0.1.0.0
newtype Distributed f rec = Distributed (forall x. Field rec x -> f x)

-- |  This instance allows you to use the @OverloadedRecordDot@ with
-- distributed records.
--
-- @
-- data User = User { name :: String }
--
-- mkRecord ''User
--
-- userIO :: Distributed IO User
-- userIO = buildDistributed \\case
--     UserName -> getLine
--
-- main :: IO ()
-- main = do
--     userName <- userIO.name
--     putStrLn userName
-- @
--
-- @since 0.1.0.0
instance (SymbolToField sym rec typ) => HasField sym (Distributed f rec) (f typ) where
    getField = getRecordFieldDistributed (symbolToField @sym @rec @typ)

-- | Use a @'Field' rec ty@ to access that field in the @'Distributed' f rec@.
--
-- @since 0.1.0.0
getRecordFieldDistributed
    :: (Record rec) => Field rec ty -> Distributed f rec -> f ty
getRecordFieldDistributed field (Distributed k) =
    k field

-- | Given a function that specifies how to construct a field of a @record@
-- wrapped in a type @f@, this constructs a @'Distributed' f record@.
--
-- @since 0.1.0.0
buildDistributed
    :: forall f rec
     . (forall ty. Field rec ty -> f ty)
    -> Distributed f rec
buildDistributed k = Distributed k

-- | Takes a 'Record' and creates a pure 'Distributed' record over any
-- 'Applicative'.
--
-- @since 0.1.0.0
distribute
    :: forall f rec. (Applicative f, Record rec) => rec -> Distributed f rec
distribute rec = Distributed \field ->
    pure (getRecordField field rec)

-- | Like 'distribute', but the record is already wrapped in a 'Functor' f.
--
-- @since 0.1.0.0
distributeF :: (Functor f, Record rec) => f rec -> Distributed f rec
distributeF fRec = Distributed $ \field ->
    fmap (getRecordField field) fRec

-- | Remove the 'Distributed' wrapper, providing an @f rec@ that can be
-- used directly.
--
-- @since 0.1.0.0
sequenceDistributedA
    :: (Applicative f, Record rec)
    => Distributed f rec
    -> f rec
sequenceDistributedA (Distributed fn) = tabulateRecordA fn

-- | Like 'sequenceDistributedA', but works on the 'Apply' class, which
-- allows for semigroup-only construction.
--
-- @since 0.1.0.0
sequenceDistributedApply :: (Apply f, Record rec) => Distributed f rec -> f rec
sequenceDistributedApply (Distributed fn) = tabulateRecordApply fn

-- | This function allows you to combine the two type wrappers into
-- a third. This merges the two records based on how you combine their
-- merging functions.
--
-- @since 0.1.0.0
zipWithDistributed
    :: (forall x. f x -> g x -> h x)
    -> Distributed f rec
    -> Distributed g rec
    -> Distributed h rec
zipWithDistributed zipFn (Distributed lhs) (Distributed rhs) = Distributed $ \field ->
    zipFn (lhs field) (rhs field)

-- | An example use of this module. The functor @'Const' 'Text'@ here means
-- that each field value is ignored, and instead we have a 'Text' value in
-- place of that record.
--
-- @since 0.1.0.0
distributedRecordFieldNames :: (Record rec) => Distributed (Const Text) rec
distributedRecordFieldNames = buildConst recordFieldLabel

-- | If you've used 'buildConst' to convert all record fields into a single
-- type, then you can use 'distributedToSemigroup' to tag each value with
-- the field name that it came from.
--
-- @since 0.1.0.0
distributedToSemigroup
    :: (Record rec, forall a. Semigroup (f a), Applicative f)
    => Distributed (Const a) rec
    -> f (Text, a)
distributedToSemigroup rec =
    getConst $
        sequenceDistributedApply $
            zipWithDistributed
                (\(Const fieldName) (Const a) -> Const (pure (fieldName, a)))
                distributedRecordFieldNames
                rec

-- | Like 'buildDistributed' but it wraps the result in 'Const'. The result
-- is a @record@ but accessing the field is
--
-- @since 0.1.0.0
buildConst
    :: forall a rec. (forall x. Field rec x -> a) -> Distributed (Const a) rec
buildConst getValue =
    buildDistributed (Const . getValue)
