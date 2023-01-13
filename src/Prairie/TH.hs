{-# language CPP #-}

-- | Helpers for generating instances of the 'Record' type class.
--
-- @since 0.0.1.0
module Prairie.TH where

import Data.Constraint (Dict(..))
import Language.Haskell.TH
import Control.Lens (lens)
import qualified Data.List as List
import Data.Traversable (for)
import Data.Char (toUpper, toLower)
import qualified Data.Text as Text

import Prairie.Class

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
mkRecord u = do
  ty <- reify u
  (typeName, con) <-
    case ty of
      TyConI dec ->
        case dec of
          DataD _cxt name _tyvars _mkind [con] _derivs ->
            pure (name, con)
          NewtypeD _cxt name _tyvars _mkind con _derivs ->
            pure (name, con)
          _ ->
            fail "unsupported data structure"
      _ ->
        fail "unsupported type"

  let
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
      RecC conName varBangTypes ->
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
            (NormalB $
            VarE 'lens
            `AppE` VarE fieldName
            `AppE`
              LamE [VarP recVar, VarP newVal]
                (RecUpdE (VarE recVar) [(fieldName, VarE newVal)])
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
    let body =
            List.foldl'
                (\acc (n, _) ->
                    VarE '(<*>)
                        `AppE` acc
                        `AppE` (VarE fromFieldName `AppE` ConE (mkConstrFieldName n))
                )
                (VarE 'pure `AppE` ConE recordCon)
                names'types

    pure $
      FunD 'tabulateRecordA
        [ Clause [VarP fromFieldName] (NormalB body) []
        ]

  mkRecordFieldLabel <- do
    fieldName <- newName "fieldName"
    body <- pure $
      CaseE (VarE fieldName)  $
        flip map names'types $ \(n, _) ->
          let
            constrFieldName =
              mkConstrFieldName n
            pat =
                compatConP constrFieldName
            bdy =
              AppE (VarE 'Text.pack) $ LitE $ StringL $ nameBase $ stripTypeName n

           in
            Match pat (NormalB bdy)  []
    pure $
      FunD 'recordFieldLabel
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
        (ConT ''Field `AppT` ConT typeName `AppT` typ)

    recordInstance =
      InstanceD
        Nothing
        []
        (ConT ''Record `AppT` ConT typeName)
        (
          [ DataInstD
              []
              Nothing
              (ConT ''Field `AppT` ConT typeName `AppT` VarT (mkName "a"))
              Nothing
              fieldConstrs
              []
          , recordFieldLensDec
          , mkTabulateRecord
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
        (ConT ''FieldDict `AppT` VarT constraintVar `AppT` ConT typeName)
        fieldDictDecl

  symbolToFieldInstances <-
    fmap concat $ for names'types $ \(fieldName, typ) -> do
      [d|
        instance SymbolToField $(litT (strTyLit (nameBase fieldName))) $(conT typeName) $(pure typ) where
          symbolToField = $(conE (mkConstrFieldName fieldName))
        |]

  pure $
    [ recordInstance
    , fieldDictInstance
    ]
    ++
      symbolToFieldInstances

overFirst :: (Char -> Char) -> String -> String
overFirst f str =
  case str of
    [] -> []
    (c:cs) -> f c : cs

upperFirst, lowerFirst :: String -> String
upperFirst = overFirst toUpper
lowerFirst = overFirst toLower

compatConP :: Name -> Pat
#if MIN_VERSION_template_haskell(2,18,0)
compatConP constrFieldName =
    ConP constrFieldName [] []
#else
compatConP constrFieldName =
    ConP constrFieldName []
#endif
