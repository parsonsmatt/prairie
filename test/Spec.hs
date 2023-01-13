{-# language PolyKinds, LambdaCase, TypeApplications, RankNTypes, StandaloneDeriving, ConstraintKinds, TemplateHaskell, DataKinds, OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, TypeFamilies, GADTs #-}
{-# options_ghc -Wall #-}
module Main where

import Prairie

import Data.Aeson
import Control.Monad
import Control.Lens

data User = User { name :: String, age :: Int }
  deriving Eq

mkRecord ''User

deriving instance Eq (Field User a)

example = User "Alice" 30

data T a = T { x :: a, y :: Int }

instance Record (T a) where
    data Field (T a) t where
        TX :: Field (T a) a
        TY :: Field (T a) Int

    recordFieldLens = \case
        TX -> lens x (\o n -> o { x = n })
        TY -> lens y (\o n -> o { y = n })

    tabulateRecordA f = T <$> f TX <*> f TY

    recordFieldLabel = \case
        TX -> "TX"
        TY -> "TY"


class PolyLens t where
    polyLens :: (forall x. Field (t x) x) -> Lens (t a) (t b) a b

instance PolyLens T where
    polyLens = \case
        TX -> lens x (\o n -> o { x = n }) :: Lens (T a) (T b) a b

type family FieldLens (a :: *) (p :: *) (f :: * -> *) where
    FieldLens (Field (t x) x) y f = LensLike f (t x) (t y) x y
    FieldLens (Field (t x) y) y f = LensLike f (t x) (t x) y y

assert :: String -> Bool -> IO ()
assert message success = unless success (error message)

main :: IO ()
main = do
    assert "getField" $ getRecordField UserName example == "Alice"
    assert "setField" $ setRecordField UserAge 32 example == User "Alice" 32
    assert "label" $ recordFieldLabel UserAge == "age"
    assert "label" $ recordFieldLabel UserName == "name"

    let t :: T Int
        t = T 3 2

        t' :: T Char
        t' = t & polyLens TX .~ 'a'

    assert "update json" $
        encode (diffRecord example (setRecordField UserName "Bob" example))
        ==
        "[{\"field\":\"name\",\"value\":\"Bob\"}]"

    assert "decode update" $
      decode "[{\"field\":\"name\",\"value\":\"Bob\"}]"
      ==
      Just [SetField UserName "Bob"]

    user' <-
      tabulateRecordA $ \case
          UserName ->
              print 10 >> pure "Matt"
          UserAge ->
              print 20 >> pure 33
    assert "tabulateRecordA" $
        user'
        ==
        User
            { name = "Matt"
            , age = 33
            }


