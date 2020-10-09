-- |  This cl
--
-- @since 0.0.1.0
module Prairie.Update where

import Data.Aeson (ToJSON(..), FromJSON(..), object, withObject, (.:), (.=))
import Control.Lens (set)
import Data.Typeable (Typeable, (:~:)(..), eqT)

import Prairie.Class

-- | An operation representing an update against the 'rec' in question.
--
-- This type is partially an example - you may want to have a more
-- sophisticated update type than merely setting fields.
--
-- @since 0.0.1.0
data Update rec where
  SetField :: Field rec a -> a -> Update rec

-- |
--
-- @since 0.0.1.0
instance (forall a. Eq (Field rec a), FieldDict Typeable rec, FieldDict Eq rec) => Eq (Update rec) where
  SetField f0 (a0 :: a0) == SetField f1 (a1 :: a1) =
    withFieldDict @Typeable f0 $
    withFieldDict @Typeable f1 $
    case eqT @a0 @a1 of
      Nothing ->
        False
      Just Refl ->
        withFieldDict @Eq f0 $
        f0 == f1 && a0 == a1

-- |
--
-- @since 0.0.1.0
instance (forall a. Show (Field rec a), FieldDict Show rec) => Show (Update rec) where
  showsPrec d (SetField field a) =
    withFieldDict @Show field $
      showParen (d > 10)
        $ showString "Update "
        . showsPrec 11 field
        . showString " "
        . showsPrec 11 a

-- |  Renders an 'Update' in the following format:
--
-- @
-- {
--    "field": 'toJSON' field,
--    "value": 'toJSON' newValue
--
-- }
-- @
--
-- @since 0.0.1.0
instance (FieldDict ToJSON rec, forall a. ToJSON (Field rec a)) => ToJSON (Update rec) where
  toJSON (SetField field newVal) =
    withFieldDict @ToJSON field $
      object [ "field" .= field, "value" .= newVal ]

-- | Parses an 'Update' with the following format:
--
-- @
-- {
--     "field": field,
--     "value": newValue
-- }
-- @
--
--
-- @since 0.0.1.0
instance (FieldDict FromJSON  rec, FieldDict Typeable rec, FromJSON (SomeField rec)) => FromJSON (Update rec) where
  parseJSON = withObject "Update" $ \o -> do
    field <- o .: "field"
    case field of
      SomeField field ->
        withFieldDict @FromJSON field $
          SetField field <$> o .: "value"

-- | Run an 'Update' against the record it is for.
--
-- @
-- >>> let user = User { name = "Bob", age = 30 }
-- >>> updateSingleField (SetField UserName "Alice") user
-- User { name = "Alice", age = 30 }
-- @
--
-- @since 0.0.1.0
updateSingleField :: Record rec => Update rec -> rec -> rec
updateSingleField (SetField field newValue) rec =
  set (recordFieldLens field) newValue rec

-- | Perform an list of updates against the 'Record'.
--
-- @since 0.0.1.0
updateRecord :: Record rec => [Update rec] -> rec -> rec
updateRecord upds rec = foldr updateSingleField rec upds
