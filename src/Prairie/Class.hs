-- | This module defines the type class 'Record' which enables much of the
-- functionality of the library. You can define instances of this record
-- manually, or you may use the @TemplateHaskell@ deriving function in
-- "Prairie.TH".
--
-- We'll use an example type @User@ throughout the documentation in this
-- module.
--
-- @
-- data User = User
--  { name :: String
--  , age :: Int
--  }
-- @
--
-- @since 0.0.1.0
module Prairie.Class where

import Control.Lens (Lens', set, view, Identity(..), Const(..))
import Data.Aeson (ToJSON(..), FromJSON(..), withText)
import Data.Constraint (Dict(..))
import Data.Kind (Constraint, Type)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable ((:~:)(..), Typeable, eqT)
import GHC.OverloadedLabels (IsLabel(..))
import GHC.TypeLits (Symbol)

-- | Instances of this class have a datatype 'Field' which allow you to
-- represent fields as a concrete datatype. This allows you to have
-- significant flexibility in working with data.
--
-- @since 0.0.1.0
class Record rec where
    -- | A datatype representing fields on the record.
    --
    -- This will be a @GADT@ with one constructor for each record field. By
    -- convention, it's best to name the constructors as with the type name
    -- leading and the field name following. This convention prevents any
    -- possible conflicts from different instances of 'Field'.
    --
    -- Using our example type @User@, we would define this as:
    --
    -- @
    -- data Field User ty where
    --   UserName :: Field User String
    --   UserAge :: Field User Int
    -- @
    --
    -- Now, we have a value @UserName@ that corresponds with the @name@ field
    -- on a @User@. The type of @User@ and @name@ are interesting to compare:
    --
    -- @
    -- UserName :: Field User    String
    -- name     ::       User -> String
    -- @
    --
    -- @since 0.0.1.0
    data Field rec :: Type -> Type

    -- | Given a 'Field' on the record, this function acts as a 'Lens'' into
    -- the record. This allows you to use a 'Field' as a getter or setter.
    --
    -- An example implementation for our 'User' type might look like this:
    --
    -- @
    -- recordFieldLens field =
    --   case field of
    --     UserName ->
    --       'lens' name (\\u n -> u { name = n })
    --     UserAge ->
    --      'lens' age (\\u a -> u { age = a })
    -- @
    --
    -- If you have derived lenses (either from Template Haskell or
    -- @generic-lens@, then you can provide those directly.
    --
    -- @since 0.0.1.0
    recordFieldLens :: Field rec ty -> Lens' rec ty

    -- |  Construct a 'Record' by providing an 'Applicative' action
    -- returning a value for each 'Field' on the 'Record'.
    --
    -- Example:
    --
    -- @
    -- tabulateRecordA $ \\field -> case field of
    --     UserName -> Just "Matt"
    --     UserAge -> Nothing
    --
    -- tabulateRecordA $ \\field -> case field of
    --     UserName -> getLine
    --     UserAge -> do
    --         ageStr <- getLine
    --         case readMaybe ageStr of
    --             Nothing -> fail $ "Expected Int, got: " <> ageStr
    --             Just a -> pure a
    -- @
    --
    -- @since 0.0.2.0
    tabulateRecordA :: Applicative m => (forall ty. Field rec ty -> m ty) -> m rec

    -- | Assign a 'Text' label for a record 'Field'.
    --
    -- This allows 'Field's to be converted to 'Text', which is useful for
    -- serialization concerns. For derserializing a 'Field', consider using
    -- @'fieldMap' :: 'Map' 'Text' ('SomeField' rec)@.
    --
    -- Record field labels can be given a @stock@ derived 'Show' instance,
    -- which works for the default implementation of the class.
    --
    -- @since 0.0.1.0
    recordFieldLabel :: Field rec ty -> Text
    default recordFieldLabel :: Show (Field rec ty) => Field rec ty -> Text
    recordFieldLabel = Text.pack . show

-- | An enumeration of fields on the record.
--
-- This value builds the fields using 'tabulateRecordA' and the 'Const'
-- type.
--
-- As of @0.0.2.0,@ this is an ordinary top-level function and not a class
-- member.
--
-- @since 0.0.1.0
allFields :: Record rec => [SomeField rec]
allFields = getConst $ tabulateRecordA $ \field ->
    Const [SomeField field]

-- | This function allows you to construct a 'Record' by providing
-- a value for each 'Field' on the record.
--
-- As of @0.0.2.0@, this is defined in terms of 'tabulateRecordA'.
--
-- @since 0.0.1.0
tabulateRecord :: Record rec => (forall ty. Field rec ty -> ty) -> rec
tabulateRecord k = runIdentity (tabulateRecordA (Identity . k))

-- | A mapping from 'Text' record field labels to the corresponding
-- 'SomeField' for that record.
--
-- @since 0.0.1.0
fieldMap :: Record rec => Map Text (SomeField rec)
fieldMap =
  foldMap
    (\sf@(SomeField f) -> Map.singleton (recordFieldLabel f) sf)
    allFields

-- | Use a 'Field' to access the corresponding value in the record.
--
-- @since 0.0.1.0
getRecordField :: Record rec => Field rec ty -> rec -> ty
getRecordField f = view (recordFieldLens f)

-- | Use a 'Field' to set the corresponding value in the record.
--
-- @since 0.0.1.0
setRecordField :: Record rec => Field rec ty -> ty -> rec -> rec
setRecordField f = set (recordFieldLens f)

-- | An existential wrapper on a 'Field'. This hides the type of the value
-- of the field. This wrapper allows you to have a collection of 'Field's
-- for a record, or to have useful instances for classes like 'Eq' where
-- the type of the values being compared must be the same.
--
-- @since 0.0.1.0
data SomeField rec where
  SomeField :: Field rec a -> SomeField rec

-- | You can write a standalone deriving instance for 'Field':
--
-- @
-- deriving stock instance 'Show' ('Field' User a)
-- @
--
-- This instance is derived, so it'll result in:
--
-- @
-- >>> show (SomeField UserAge)
-- SomeField UserAge
-- @
--
-- @since 0.0.1.0
deriving stock instance (forall a. Show (Field rec a)) => Show (SomeField rec)

instance
  ( forall a. Eq (Field rec a)
  , FieldDict Typeable rec
  )
 =>
  Eq (SomeField rec)
 where
  SomeField (f0 :: Field rec a) == SomeField (f1 :: Field rec b) =
    withFieldDict @Typeable f0 $
    withFieldDict @Typeable f1 $
    case eqT @a @b of
      Just Refl ->
          f0 == f1
      Nothing ->
        False

-- | This instance delegates to the underlying instance of 'ToJSON' for the
-- given field.
--
-- @since 0.0.1.0
instance (forall a. ToJSON (Field rec a)) => ToJSON (SomeField rec) where
  toJSON (SomeField f) = toJSON f

-- | This instance delegates to the underlying instance of 'FromJSON' for
-- the given field.
--
-- @since 0.0.1.0
instance (Record rec) => FromJSON (SomeField rec) where
  parseJSON = withText "Field" $ \txt ->
    case Map.lookup txt (fieldMap @rec) of
      Just field ->
        pure field
      Nothing ->
        fail "Field not"

-- | This delegates to 'recordFieldLabel'  from the 'Record' class.
--
-- @since 0.0.1.0
instance Record rec => ToJSON (Field rec a) where
  toJSON = toJSON . recordFieldLabel

-- | This parses a 'Field' from a 'Text' given by the function
-- 'recordFieldLabel'.
--
-- @since 0.0.1.0
instance (Record rec, FieldDict Typeable rec, Typeable a) => FromJSON (Field rec a) where
  parseJSON = withText "Field" $ \txt ->
    case Map.lookup txt (fieldMap @rec) of
      Just (SomeField (a :: Field rec b)) ->
        withFieldDict @Typeable a $
        case eqT @a @b of
          Just Refl ->
            pure a
          Nothing ->
            fail "types not same???"
      Nothing ->
        fail "Field not"

-- | This class allows you to summon a type class instance based on a 'Field'
-- of the record. Use this type class when you need to assert that all the
-- fields of a record satisfy some type class instance.
--
-- For example, suppose we want to write a generic logging utility for all
-- records where all fields on the record is loggable.
--
-- @
-- class Loggable a where
--   toLog :: a -> LogMessage
-- @
--
-- We can implement a function based on 'Record' to log it:
--
-- @
-- logRecord :: (FieldDict Loggable rec) => rec -> LogMessage
-- logRecord record = foldMap go 'allFields'
--   where
--     go ('SomeField' field) =
--       'withFieldDict' @Loggable $
--       toLog ('getRecordField' field record)
-- @
--
-- The second parameter to 'withFieldDict' will have the instance of
-- 'Loggable a' in scope.
--
-- You can define instances polymorphic in the constraint with the
-- @ConstraintKinds@ language extension.
--
-- @since 0.0.1.0
class (Record r) => FieldDict (c :: Type -> Constraint) (r :: Type) where
  -- | Return the 'Dict' for the given field.
  --
  -- An implementation of this for the 'User' type would case on each field
  -- and return 'Dict' in each branch.
  --
  -- @
  -- getFieldDict userField =
  --   case userField of
  --    UserName -> Dict
  --    UserAge -> Dict
  -- @
  --
  -- @since 0.0.1.0
  getFieldDict :: Field r a -> Dict (c a)

-- | Given a record @field :: 'Field' rec a@, this function brings the
-- type class instance @c a@ into scope for the third argument.
--
-- This function is intended to be used with a @TypeApplication@ for the
-- constraint you want to instantiate. It is most useful for working with
-- generic records in type class instances.
--
-- @since 0.0.1.0
withFieldDict
  :: forall c rec a r
   . FieldDict c rec
  => Field rec a
  -- ^ The record field we want to unpack. We need this value in order to
  -- know what type we want the constraint to apply to.
  -> (c a => r)
  -- ^ A value that assumes the constraint @c@ holds for the type @a@.
  -> r
withFieldDict l k =
  case getFieldDict @c l of
    Dict -> k

-- | This type class enables you to map a 'Symbol's to a record 'Field'.
--
-- To use this, you'll define an instance
--
-- @
-- instance 'SymbolToField' "age" User Int where
--   symbolToField = UserAge
--
-- instance 'SymbolToField' "name" User String where
--   symbolToField = UserName
-- @
--
-- The main utility here is that you can then write @OverloadedSymbols@
-- that correspond to record fields.
--
-- @
-- nameField :: ('SymbolToField'' "name" rec a) => 'Field' rec a
-- nameField = #name
--
-- userNameField :: 'Field' User String
-- userNameField = #name
-- @
--
-- Note that there's nothing forcing you to use a symbol that exactly
-- matches the type. You can write multiple instances of this for each
-- constructor. The following two instances are perfectly happy to live
-- together.
--
-- @
-- instance 'SymbolToField' "name" User String where
--   symbolToField = UserName
--
-- instance 'SymbolToField' "userName" User String where
--   symbolToField = UserName
-- @
--
-- @since 0.0.1.0
class Record rec => SymbolToField (sym :: Symbol) (rec :: Type) (a :: Type) | rec sym -> a where
  -- | This function is designed to be used with a type application:
  --
  -- @
  -- symbolToField @"age"
  -- @
  --
  -- @since 0.0.1.0
  symbolToField :: Field rec a

-- | If you've defined the relevant instances for 'SymbolToField', then you
-- can use @OverloadedLabels@ to write your labels. This can be convenient
-- if you want to avoid a lot of duplication and verbosity in the types.
--
-- Given the instances for the @User@ example type, we are able to write:
--
-- @
-- getUserName :: User -> String
-- getUserName = getRecordField #name
-- @
--
-- @since 0.0.1.0
instance (SymbolToField sym rec a) => IsLabel sym (Field rec a) where
  fromLabel = symbolToField @sym
