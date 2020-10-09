{-# language ConstraintKinds, TemplateHaskell, DataKinds, OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, TypeFamilies, GADTs #-}
module Main where

import Prairie

import Control.Monad

data User = User { name :: String, age :: Int }
  deriving Eq

mkRecord ''User

example = User "Alice" 30

assert :: String -> Bool -> IO ()
assert message success = unless success (error message)

main :: IO ()
main = do
  assert "getField" $ getRecordField UserName example == "Alice"
  assert "setField" $ setRecordField UserAge 32 example == User "Alice" 32
  assert "label" $ recordFieldLabel UserAge == "age"
  assert "label" $ recordFieldLabel UserName == "name"
