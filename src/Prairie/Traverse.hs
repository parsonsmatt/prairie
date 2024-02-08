-- | This module shows how to 'traverse' over a 'Record', allowing you to
-- perform side-effects for each field and return a transformed value.
--
-- @since 0.0.3.0
module Prairie.Traverse where

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
    :: forall rec f. (Record rec, Applicative f)
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
