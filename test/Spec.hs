{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Prairie

import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Kind (Type)
import Data.Monoid
import GHC.Records
import Prairie.AsRecord
import Test.Hspec

data User = User {name :: String, age :: Int}
    deriving (Show, Eq)

mkRecord ''User

deriving instance Eq (Field User a)
deriving instance Show (Field User a)

exampleUser = User "Alice" 30

data Foo = Foo
    { ints :: [Int]
    , char :: First Char
    }
    deriving (Show, Eq)

mkRecord ''Foo

deriving via AsRecord Foo instance Semigroup Foo
deriving via AsRecord Foo instance Monoid Foo

data T a = T {x :: a, y :: Int}

instance Record (T a) where
    data Field (T a) t where
        TX :: Field (T a) a
        TY :: Field (T a) Int

    recordFieldLens = \case
        TX -> lens x (\o n -> o{x = n})
        TY -> lens y (\o n -> o{y = n})

    tabulateRecordA f = T <$> f TX <*> f TY

    recordFieldLabel = \case
        TX -> "TX"
        TY -> "TY"

class PolyLens t where
    polyLens :: (forall x. Field (t x) x) -> Lens (t a) (t b) a b

instance PolyLens T where
    polyLens = \case
        TX -> lens x (\o n -> o{x = n}) :: Lens (T a) (T b) a b

type family FieldLens (a :: Type) (p :: Type) (f :: Type -> Type) where
    FieldLens (Field (t x) x) y f = LensLike f (t x) (t y) x y
    FieldLens (Field (t x) y) y f = LensLike f (t x) (t x) y y

main :: IO ()
main = hspec $ do
    describe "Prairie" $ do
        it "getField" $ do
            getRecordField UserName exampleUser `shouldBe` "Alice"
        it "setField" $ do
            setRecordField UserAge 32 exampleUser
                `shouldBe` User{name = "Alice", age = 32}
        it "label" $ do
            recordFieldLabel UserAge `shouldBe` "age"
        it "label" $ do
            recordFieldLabel UserName `shouldBe` "name"

        let
            t :: T Int
            t = T 3 2

            t' :: T Char
            t' = t & polyLens TX .~ 'a'

        it "update json" $
            encode (diffRecord exampleUser (setRecordField UserName "Bob" exampleUser))
                `shouldBe` "[{\"field\":\"name\",\"value\":\"Bob\"}]"

        it "decode update" $
            decode "[{\"field\":\"name\",\"value\":\"Bob\"}]"
                `shouldBe` Just [SetField UserName "Bob"]

        it "tabulateRecordA" $ do
            user' <-
                tabulateRecordA $ \case
                    UserName ->
                        print 10 >> pure "Matt"
                    UserAge ->
                        print 20 >> pure 33
            user'
                `shouldBe` User
                    { name = "Matt"
                    , age = 33
                    }

        describe "Fold" $ do
            describe "foldRecord" $ do
                it "can count the fields" $ do
                    foldRecord (\_val acc _field -> acc + 1) 0 exampleUser
                        `shouldBe` 2
                it "can distinguish fields" $ do
                    foldRecord
                        ( \val acc field ->
                            case field of
                                UserName ->
                                    length val + acc
                                UserAge ->
                                    val + acc
                        )
                        (0 :: Int)
                        exampleUser
                        `shouldBe` 35

            describe "foldMapRecord" $ do
                it "can count the fields" $ do
                    foldMapRecord (\_ _ -> Sum 1) exampleUser
                        `shouldBe` Sum 2
                it "can combine strings" $ do
                    foldMapRecord
                        ( \val ->
                            \case
                                UserName -> val
                                UserAge -> show val
                        )
                        exampleUser
                        `shouldBe` ("Alice30" :: String)

        describe "Traverse" $ do
            it "can validate a record" $ do
                let
                    validateField :: ty -> Field User ty -> Maybe ty
                    validateField val =
                        \case
                            UserName -> do
                                guard (length val >= 1)
                                pure val
                            UserAge -> do
                                guard (val >= 18)
                                pure val
                traverseRecord validateField exampleUser
                    `shouldBe` Just User{name = "Alice", age = 30}

                traverseRecord validateField User{name = "", age = 1}
                    `shouldBe` Nothing

            it "can do either" $ do
                let
                    validateField :: ty -> Field User ty -> Either String ty
                    validateField val =
                        \case
                            UserName -> do
                                if length val >= 1
                                    then pure val
                                    else Left "Name must be at least one character"
                            UserAge -> do
                                if val >= 18
                                    then pure val
                                    else Left "Age must be at least 18"

                traverseRecord validateField exampleUser
                    `shouldBe` Right User{name = "Alice", age = 30}

                traverseRecord validateField User{name = "", age = 1}
                    `shouldBe` Left "Age must be at least 18"

            it "can target one field" $ do
                traverseRecord
                    ( \val ->
                        \case
                            UserName -> do
                                guard (length val >= 1)
                                pure val
                            _ ->
                                pure val
                    )
                    User{name = "", age = 30}
                    `shouldBe` Nothing

        describe "Semigroup" do
            it "can combine two records" do
                let
                    f0 = Foo [1] (First Nothing)
                    f1 = Foo [2, 3] (First (Just 'a'))
                f0 <> f1
                    `shouldBe` Foo [1, 2, 3] (First (Just 'a'))

        describe "Monoid" do
            it "can produce an empty record" do
                let
                    obvious =
                        Foo mempty mempty
                mempty `shouldBe` obvious

        describe "Zip" do
            it "can combine two records" do
                let
                    u0 = User "Matt" 35
                    u1 = User "ttaM" 53
                zipWithRecord
                    ( \a b -> \case
                        UserName ->
                            a <> b
                        UserAge ->
                            a + b
                    )
                    u0
                    u1
                    `shouldBe` User "MattttaM" (35 + 53)
