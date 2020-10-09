{-# language TypeApplications, StandaloneDeriving, ConstraintKinds, TemplateHaskell, DataKinds, OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, TypeFamilies, GADTs #-}
module Main where

import Prairie

import Data.Aeson
import Control.Monad

data User = User { name :: String, age :: Int }
  deriving Eq

mkRecord ''User

deriving instance Eq (Field User a)

example = User "Alice" 30

assert :: String -> Bool -> IO ()
assert message success = unless success (error message)

main :: IO ()
main = do
  assert "getField" $ getRecordField UserName example == "Alice"
  assert "setField" $ setRecordField UserAge 32 example == User "Alice" 32
  assert "label" $ recordFieldLabel UserAge == "age"
  assert "label" $ recordFieldLabel UserName == "name"

  assert "update json" $
    encode (diffRecord example (setRecordField UserName "Bob" example))
    ==
    "[{\"field\":\"name\",\"value\":\"Bob\"}]"

  assert "decode update" $
    decode "[{\"field\":\"name\",\"value\":\"Bob\"}]"
    ==
    Just [SetField UserName "Bob"]
