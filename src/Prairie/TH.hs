{-# LANGUAGE CPP #-}

-- | Helpers for generating instances of the 'Record' type class.
--
-- @since 0.0.1.0
module Prairie.TH
    ( -- * Generating @Record@ instances
      mkRecord
    , mkRecordWith

      -- * Configuration
      -- $options
    , PrairieOptions
    , defaultPrairieOptions
    , useTypeEquality
    ) where

import Data.Char (toLower, toUpper)
import Data.Constraint (Dict (..))
import Data.Functor.Apply (Apply (..))
import Data.Functor.Compose
import qualified Data.List as List
import qualified Data.Text as Text
import Data.Traversable (for)
import GHC.Records (getField)
import Language.Haskell.TH

import Prairie.Class
import Prairie.Internal (lens)

-- | Create an instance of the 'Record' type class.
--
-- @
-- data User
--   = User
--   { name :: String
--   , age :: Int
--   }
--
-- mkRecord ''User
--
-- ====>
--
-- instance Record User where
--   data Field User a where
--     UserName :: String
--     UserAge :: Int
--
--   recordFieldLens fl =
--     case fl of
--       UserName -> lens name (\u n -> u { name = n)
--       UserAge -> lens age (\u n -> u { age = n)
--
-- instance SymbolToField "age" User Int where symbolToField = UserName
-- instance SymbolToField "name" User String where symbolToField = UserAge
-- @
--
-- If the fields are prefixed with the type's name, this function figures
-- it out and won't duplicate the field.
--
-- @
-- data User
--   = User
--   { userName :: String
--   , userAge :: Int
--   }
--
-- mkRecord ''User
--
-- ====>
--
-- instance Record User where
--   data Field User a where
--     UserName :: String
--     UserAge :: Int
--
--   recordFieldLens fl =
--     case fl of
--       UserName -> lens name (\u n -> u { name = n)
--       UserAge -> lens age (\u n -> u { age = n)
--
-- instance SymbolToField "name" User Int where symbolToField = UserName
-- instance SymbolToField "age" User String where symbolToField = UserAge
-- @
--
-- @since 0.0.1.0
mkRecord :: Name -> DecsQ
mkRecord = mkRecordWith defaultPrairieOptions

-- $options
--
-- 'PrairieOptions' is an opaque type. Configure it by record-updating
-- 'defaultPrairieOptions' using the exported field accessors, e.g.
--
-- @
-- 'defaultPrairieOptions' { 'useTypeEquality' = True }
-- @
--
-- The constructor is intentionally not exported so that future options can
-- be added without it being a breaking change.

-- | Options controlling how 'mkRecordWith' generates instances.
--
-- This type is abstract; see 'defaultPrairieOptions' and the individual field
-- accessors ('useTypeEquality').
--
-- @since 0.1.2.0
data PrairieOptions = PrairieOptions
    { useTypeEquality :: Bool
    -- ^ Generate the 'SymbolToField' instances using a @~@ equality
    -- constraint to bind the field type, rather than placing the field type
    -- directly in the instance head:
    --
    -- @
    -- -- with useTypeEquality = False (default):
    -- instance SymbolToField \"foo\" Rec Ty where ...
    --
    -- -- with useTypeEquality = True:
    -- instance (field ~ Ty) => SymbolToField \"foo\" Rec field where ...
    -- @
    --
    -- This admits field types that are not permitted directly in an instance
    -- head — most notably type-family applications (e.g. @field :: Family m
    -- Int@), but anything else that an instance head rejects too. The cost is
    -- that the calling module must enable the @TypeOperators@ extension, so it
    -- is off by default: the generated code is then identical to previous
    -- versions of @prairie@. When it is off and a field is found to use a type
    -- family, 'mkRecordWith' fails with a descriptive error pointing here.
    --
    -- @since 0.1.2.0
    }

-- | The default 'PrairieOptions'. 'mkRecord' is @'mkRecordWith' 'defaultPrairieOptions'@,
-- and the generated code is identical to previous versions of @prairie@.
--
-- @since 0.1.2.0
defaultPrairieOptions :: PrairieOptions
defaultPrairieOptions =
    PrairieOptions
        { useTypeEquality = False
        }

-- | Like 'mkRecord', but with control over code generation via 'PrairieOptions'.
--
-- For example, to allow fields whose type is a type-family application:
--
-- @
-- {-\# LANGUAGE TypeOperators \#-}
--
-- mkRecordWith defaultPrairieOptions { useTypeEquality = True } ''MyRecord
-- @
--
-- @since 0.1.2.0
mkRecordWith :: PrairieOptions -> Name -> DecsQ
mkRecordWith opts u = do
    ty <- reify u
    (typeName, con, tyvars) <-
        case ty of
            TyConI dec ->
                case dec of
                    DataD _cxt name tyvars _mkind [con] _derivs ->
                        pure (name, con, tyvars)
                    NewtypeD _cxt name tyvars _mkind con _derivs ->
                        pure (name, con, tyvars)
                    _ ->
                        fail "unsupported data structure"
            _ ->
                fail "unsupported type"

    let
        instanceHead = List.foldl' AppT (ConT typeName) $ fmap getTyVarType tyvars
        -- Annotate a record update with the record's type so that
        -- @DuplicateRecordFields@ can figure out which field is being set. We
        -- only do this for monomorphic records: applying the type constructor
        -- to its variables (e.g. @One a@) in an expression signature would
        -- introduce fresh, rigid type variables that cannot unify with the
        -- ones bound by the instance head. For polymorphic records the type of
        -- the surrounding 'lens' already fixes the record type, so no
        -- annotation is needed.
        annotateRecordUpdate e =
            case tyvars of
                [] -> SigE e instanceHead
                _ -> e
        stripTypeName n =
            let
                typeNamePrefix =
                    lowerFirst (nameBase typeName)
             in
                case List.stripPrefix typeNamePrefix (nameBase n) of
                    Just xs -> mkName (lowerFirst xs)
                    Nothing -> n

    (recordCon, names'types) <-
        case con of
            RecC conName varBangTypes -> do
                case varBangTypes of
                    [] ->
                        reportError "Prairie records must have at least one field."
                    _ ->
                        pure ()
                pure $ (conName, map (\(n, _b, t) -> (n, t)) varBangTypes)
            _ ->
                fail "only supports records"

    let
        mkConstrFieldName fieldName =
            mkName (nameBase typeName <> upperFirst (nameBase (stripTypeName fieldName)))

    fieldLensClause <- do
        arg <- newName "field"
        let
            mkMatch (fieldName, _typ) = do
                recVar <- newName "rec"
                newVal <- newName "newVal"

                pure $
                    Match
                        (compatConP (mkConstrFieldName fieldName))
                        ( NormalB $
                            VarE 'lens
                                `AppE` (VarE 'getField `AppTypeE` LitT (StrTyLit (nameBase fieldName)))
                                `AppE` LamE
                                    [VarP recVar, VarP newVal]
                                    ( annotateRecordUpdate
                                        (RecUpdE (VarE recVar) [(fieldName, VarE newVal)])
                                    )
                        )
                        []
        body <- CaseE (VarE arg) <$> traverse mkMatch names'types
        pure $ Clause [VarP arg] (NormalB body) []
    let
        recordFieldLensDec =
            FunD 'recordFieldLens [fieldLensClause]
        fieldConstructors =
            map (\(n, t) -> (mkConstrFieldName n, t)) names'types

    mkTabulateRecord <- do
        fromFieldName <- newName "fromField"
        let
            body =
                List.foldl'
                    ( \acc (n, _) ->
                        VarE '(<*>)
                            `AppE` acc
                            `AppE` (VarE fromFieldName `AppE` ConE (mkConstrFieldName n))
                    )
                    (VarE 'pure `AppE` ConE recordCon)
                    names'types

        pure $
            FunD
                'tabulateRecordA
                [ Clause [VarP fromFieldName] (NormalB body) []
                ]

    mkTabulateRecordApply <- do
        fromFieldName <- newName "fromField"
        let
            body =
                fst $
                    List.foldl'
                        ( \(acc, op) fromField ->
                            ( InfixE (Just acc) op (Just fromField)
                            , VarE '(<.>)
                            )
                        )
                        (ConE recordCon, VarE '(<$>))
                        fromFieldNames
            fromFieldNames =
                map (AppE (VarE fromFieldName) . ConE . mkConstrFieldName . fst) names'types

        pure $
            FunD
                'tabulateRecordApply
                [ Clause [VarP fromFieldName] (NormalB body) []
                ]

    mkSequenceRecordA <- do
        fromFieldName <- newName "s"
        let
            mkMatch (VarP lamArg) (fieldName, _typ) = do
                let
                    pat = compatConP $ mkConstrFieldName fieldName
                 in
                    Match pat (NormalB (VarE lamArg)) []
        lambda <- do
            vars <- traverse (fmap VarP . newName . nameBase . fst) names'types
            pure $
                LamE
                    vars
                    (ConE 'Distributed `AppE` LamCaseE (List.zipWith mkMatch vars names'types))
        let
            body =
                List.foldl'
                    ( \acc (n, _) ->
                        VarE '(<*>)
                            `AppE` acc
                            `AppE` (VarE 'getCompose `AppE` (VarE fromFieldName `AppE` ConE (mkConstrFieldName n)))
                    )
                    (VarE 'pure `AppE` lambda)
                    names'types

        pure $
            FunD
                'sequenceRecordA
                [ Clause [compatConP' 'Distributed [VarP fromFieldName]] (NormalB body) []
                ]

    mkRecordFieldLabel <- do
        fieldName <- newName "fieldName"
        body <- pure $
            CaseE (VarE fieldName) $
                flip map names'types $ \(n, _) ->
                    let
                        constrFieldName =
                            mkConstrFieldName n
                        pat =
                            compatConP constrFieldName
                        bdy =
                            AppE (VarE 'Text.pack) $ LitE $ StringL $ nameBase $ stripTypeName n
                     in
                        Match pat (NormalB bdy) []
        pure $
            FunD
                'recordFieldLabel
                [ Clause [VarP fieldName] (NormalB body) []
                ]

    let
        fieldConstrs =
            map mkFieldConstr fieldConstructors
        mkFieldConstr (fieldName, typ) =
            GadtC
                [ fieldName
                ]
                []
                (ConT ''Field `AppT` instanceHead `AppT` typ)

        recordInstance =
            InstanceD
                Nothing
                []
                (ConT ''Record `AppT` instanceHead)
                ( [ DataInstD
                        []
                        Nothing
                        (ConT ''Field `AppT` instanceHead `AppT` VarT (mkName "_"))
                        Nothing
                        fieldConstrs
                        []
                  , recordFieldLensDec
                  , mkTabulateRecord
                  , mkTabulateRecordApply
                  , mkSequenceRecordA
                  , mkRecordFieldLabel
                  ]
                )

    fieldDictInstance <- do
        constraintVar <- newName "c"
        fieldVar <- newName "field"
        let
            allFieldsC =
                map (VarT constraintVar `AppT`) (map snd names'types)
            fieldDictDecl =
                [ FunD 'getFieldDict [Clause [VarP fieldVar] (NormalB fieldDictBody) []]
                ]
            fieldDictBody =
                CaseE (VarE fieldVar) $ map mkFieldDictMatches fieldConstructors
            mkFieldDictMatches (name, _type) =
                Match (compatConP name) (NormalB (ConE 'Dict)) []

        pure $
            InstanceD
                Nothing -- maybe overlap
                allFieldsC
                (ConT ''FieldDict `AppT` VarT constraintVar `AppT` instanceHead)
                fieldDictDecl

    let
        -- Bind the field type through a @~@ equality constraint. Needed for
        -- field types that cannot appear directly in an instance head (e.g.
        -- type-family applications); requires @TypeOperators@ at the use site.
        equalitySymbolToField fieldName typ = do
            retType <- newName "field"
            [d|
                instance
                    ($(varT retType) ~ $(pure typ))
                    => SymbolToField
                        $(litT (strTyLit (nameBase fieldName)))
                        $(pure instanceHead)
                        $(varT retType)
                    where
                    symbolToField = $(conE (mkConstrFieldName fieldName))
                |]
        -- Place the field type directly in the instance head, exactly as
        -- @prairie@ always has (no @TypeOperators@ required).
        directSymbolToField fieldName typ =
            [d|
                instance
                    SymbolToField
                        $(litT (strTyLit (nameBase fieldName)))
                        $(pure instanceHead)
                        $(pure typ)
                    where
                    symbolToField = $(conE (mkConstrFieldName fieldName))
                |]

    symbolToFieldInstances <-
        fmap concat $ for names'types $ \(fieldName, typ) ->
            if useTypeEquality opts
                then equalitySymbolToField fieldName typ
                else do
                    -- The direct form can't express a type-family field. Detect
                    -- that and fail with guidance rather than letting GHC emit a
                    -- confusing instance-head error.
                    mentionsFamily <- typeMentionsFamily typ
                    if mentionsFamily
                        then fail (typeFamilyFieldError typeName fieldName typ)
                        else directSymbolToField fieldName typ

    pure $
        [ recordInstance
        , fieldDictInstance
        ]
            ++ symbolToFieldInstances

-- | Does this type mention a type or data family anywhere within it? Such
-- types are not permitted in instance heads, so the generated
-- 'SymbolToField' instance has to bind them through an equality constraint
-- instead (see 'allowTypeFamilies'). The walk short-circuits as soon as a
-- family is found, and expands type synonyms so that a synonym hiding a
-- family (e.g. @type Foo m = Family m Int@) is still detected.
typeMentionsFamily :: Type -> Q Bool
typeMentionsFamily = go
  where
    go ty =
        case ty of
            ConT n -> isFamily n
            InfixT a n b -> anyM [isFamily n, go a, go b]
            UInfixT a n b -> anyM [isFamily n, go a, go b]
            AppT a b -> anyM [go a, go b]
            AppKindT a _ -> go a
            SigT a _ -> go a
            ParensT a -> go a
            _ -> pure False

    isFamily n = do
        info <- reify n
        case info of
            FamilyI{} -> pure True
            -- Type synonyms are not recursive, so this terminates.
            TyConI (TySynD _ _ rhs) -> go rhs
            _ -> pure False

    anyM =
        foldr
            (\m acc -> m >>= \found -> if found then pure True else acc)
            (pure False)

-- | The error reported when a field uses a type family but
-- 'useTypeEquality' has not been enabled.
typeFamilyFieldError :: Name -> Name -> Type -> String
typeFamilyFieldError tyName fieldName typ =
    unlines
        [ "prairie: the field `"
            <> nameBase fieldName
            <> "` of `"
            <> nameBase tyName
            <> "` has a type that mentions a type family:"
        , ""
        , "    " <> pprint typ
        , ""
        , "Generating instances for such a field requires binding the field type"
        , "with a `~` equality constraint, which means the calling module must"
        , "enable the TypeOperators extension. Because that is a new requirement,"
        , "it is opt-in. Enable it like so:"
        , ""
        , "    {-# LANGUAGE TypeOperators #-}"
        , ""
        , "    mkRecordWith defaultPrairieOptions { useTypeEquality = True } ''"
            <> nameBase tyName
        ]

overFirst :: (Char -> Char) -> String -> String
overFirst f str =
    case str of
        [] -> []
        (c : cs) -> f c : cs

upperFirst, lowerFirst :: String -> String
upperFirst = overFirst toUpper
lowerFirst = overFirst toLower

-- | @template-haskell-2.17@ (GHC 9.0) added a @flag@ parameter to
-- 'TyVarBndr'; on older versions the constructors have one fewer field.
#if MIN_VERSION_template_haskell(2,17,0)
getTyVarType :: TyVarBndr a -> Type
getTyVarType (PlainTV n _) = VarT n
getTyVarType (KindedTV n _ _) = VarT n
#else
getTyVarType :: TyVarBndr -> Type
getTyVarType (PlainTV n) = VarT n
getTyVarType (KindedTV n _) = VarT n
#endif

compatConP :: Name -> Pat
#if MIN_VERSION_template_haskell(2,18,0)
compatConP constrFieldName =
    ConP constrFieldName [] []
#else
compatConP constrFieldName =
    ConP constrFieldName []
#endif

compatConP' :: Name -> [Pat] -> Pat
#if MIN_VERSION_template_haskell(2,18,0)
compatConP' constrFieldName ps =
    ConP constrFieldName [] ps
#else
compatConP' constrFieldName ps  =
    ConP constrFieldName ps
#endif
