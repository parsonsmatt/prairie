-- | This module shows how to 'traverse' over a 'Record', allowing you to
-- perform side-effects for each field and return a transformed value.
--
-- @since 0.0.3.0
module Prairie.Traverse where

import Control.Applicative (liftA2)
import Data.Foldable (traverse_)
import Data.List (foldl')
import Prairie.Class
import Prairie.Fold

-- | Apply an effectful function over each field of a 'Record', producing
-- a new 'Record' in the process.
--
-- This example use increments a @User@s age and requests a new name.
--
-- @
-- happyBirthday :: User -> IO User
-- happyBirthday =
--     traverseRecord
--          (\\val field ->
--              case field of
--                  UserName -> do
--                      putStrLn $ "Current name is: " <> val
--                      putStrLn "Please input a new name: "
--                      getLine
--                  UserAge -> do
--                      putStrLn $ "Current age is " <> show val
--                      pure (val + 1)
--          )
-- @
--
-- If you only want to target a single field, you can use a wildcard match
-- and `pure`. This example also uses @LambdaCase@.
--
-- @
-- nameAtLeastOneCharacter :: User -> 'Maybe' User
-- nameAtLeastOneCharacter =
--      'traverseRecord'
--          (\val ->
--              \case
--                  UserName -> do
--                      'guard' ('length' val >= 1)
--                      'pure' val
--                  _ ->
--                      'pure' val
--          )
-- @
--
-- @since 0.0.3.0
traverseRecord
    :: forall rec f
     . (Record rec, Applicative f)
    => (forall ty. ty -> Field rec ty -> f ty)
    -> rec
    -> f rec
traverseRecord f init =
    foldl' k (pure init) (recordToFieldList init)
  where
    f' :: Field rec ty -> ty -> f (SomeFieldWithValue rec)
    f' field val =
        SomeFieldWithValue field <$> f val field
    k frec (SomeFieldWithValue field val) =
        liftA2
            (\(SomeFieldWithValue field' val') rec -> setRecordField field' val' rec)
            (f' field val)
            frec

-- | Like 'traverseRecord', but throws away it's result. This is slightly
-- more efficient than @'void' $ 'traverseRecord' f@.
--
-- @since 0.1.0.0
traverseRecord_
    :: forall rec f
     . (Record rec, Applicative f)
    => (forall ty. ty -> Field rec ty -> f ())
    -> rec
    -> f ()
traverseRecord_ f init =
    traverse_
        (\(SomeFieldWithValue field val) -> f val field)
        (recordToFieldList init)

-- | This function produces a container resulting from running the given
-- function on every field of a record.
--
-- @since 0.1.0.0
traverseFields
    :: forall rec m f a
     . ( Record rec
       , Applicative m
       , forall x. Semigroup (f x)
       , Traversable f
       , Applicative f
       )
    => (forall ty. Field rec ty -> m a)
    -> m (f a)
traverseFields k =
    traverse (\(SomeField f) -> k f) (allFields @rec)

-- | Like 'traverseFields', but just runs the given action on every field.
-- Does not return any values, so only useful for enumerating side-effects.
--
-- @since 0.1.0.0
traverseFields_
    :: forall rec m
     . (Record rec, Applicative m)
    => (forall ty. Field rec ty -> m ())
    -> m ()
traverseFields_ f =
    traverse_ (\(SomeField field) -> f field) (allFields @rec @[])

-- | Collect results of running the provided callback over each field, with
-- a specific constraint being allowed on each field. Allows you to operate
-- somewhat generically over the record fields.
--
-- @since 0.1.0.0
traverseFieldsWithDict
    :: forall r c f t a
     . ( FieldDict c r
       , Applicative f
       , forall a. Semigroup (t a)
       , Applicative t
       , Traversable t
       )
    => (forall x. (c x) => Field r x -> f a)
    -> f (t a)
traverseFieldsWithDict f =
    traverse
        ( (\(SomeField field) -> withFieldDict @c @r field (f field))
            :: SomeField r -> f a
        )
        allFields

-- | Lke 'traverseFieldsWithDict', but throws away the result values.
-- Useful when you want to enumerate all fields and perform
-- a side-effecting function for each field.
--
-- @since 0.1.0.0
traverseFieldsWithDict_
    :: forall r c f
     . (FieldDict c r, Applicative f)
    => (forall x. (c x) => Field r x -> f ())
    -> f ()
traverseFieldsWithDict_ f =
    traverse_
        ( (\(SomeField field) -> withFieldDict @c @r field (f field))
            :: SomeField r -> f ()
        )
        (allFields @r @[])
