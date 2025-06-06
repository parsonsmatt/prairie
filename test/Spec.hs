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
{-# OPTIONS_GHC -Wall -Werror=unused-type-patterns #-}

module Main where

import Prairie

import Control.Lens hiding ((<.>))
import Control.Monad
import Data.Aeson
import Data.Functor.Apply (Apply (..))
import Data.Functor.Compose
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid
import Prairie.AsRecord
import Test.Hspec

data User = User {name :: String, age :: Int}
    deriving (Show, Eq)

mkRecord ''User

deriving instance Eq (Field User a)
deriving instance Show (Field User a)

exampleUser :: User
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
    data Field (T a) _ where
        TX :: Field (T a) a
        TY :: Field (T a) Int

    recordFieldLens = \case
        TX -> lens x (\o n -> o{x = n})
        TY -> lens y (\o n -> o{y = n})

    tabulateRecordA f = T <$> f TX <*> f TY

    tabulateRecordApply f = (T <$> f TX) <.> f TY

    recordFieldLabel = \case
        TX -> "TX"
        TY -> "TY"

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
                        print @Int 10 >> pure "Matt"
                    UserAge ->
                        print @Int 20 >> pure 33
            user'
                `shouldBe` User
                    { name = "Matt"
                    , age = 33
                    }

        it "tabulateRecordApply" do
            user' <-
                tabulateRecordApply $ \case
                    UserName ->
                        print (10 :: Int) >> pure "Matt"
                    UserAge ->
                        print (20 :: Int) >> pure 33
            user'
                `shouldBe` User
                    { name = "Matt"
                    , age = 33
                    }

        it "sequenceRecordA" $ do
            Distributed user' <- do
                sequenceRecordA @User @IO @Maybe $ Distributed \case
                    UserName -> Compose do
                        print @Int 10 >> pure (Just "Matt")
                    UserAge -> Compose do
                        print @Int 20 >> pure Nothing
            user' UserName `shouldBe` Just "Matt"
            user' UserAge `shouldBe` Nothing
            tabulateRecordA user' `shouldBe` Nothing

        describe "Fold" $ do
            describe "foldRecord" $ do
                it "can count the fields" $ do
                    foldRecord (\_val acc _field -> acc + 1) 0 exampleUser
                        `shouldBe` (2 :: Int)
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
                it "can make a nonempty list" $ do
                    foldMapRecord (\_ _ -> pure 'a') exampleUser
                        `shouldBe` ('a' :| "a")
                it "can count the fields" $ do
                    foldMapRecord (\_ _ -> Sum 1) exampleUser
                        `shouldBe` Sum (2 :: Integer)
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
