{-# language NoFieldSelectors #-}
{-# language DuplicateRecordFields #-}
{-# language TemplateHaskell #-}
{-# language DataKinds #-}
{-# language TypeFamilies #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language UndecidableInstances #-}
{-# language GADTs #-}

module Prairie.NoFieldSelectorSpec where

import Prairie

data User = User { name :: String }

mkRecord ''User

data Dog = Dog { name :: String }

mkRecord ''Dog
