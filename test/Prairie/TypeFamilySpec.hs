{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Prairie.TypeFamilySpec where

import Prairie

data Mode = Foo | Bar

type family Family (m :: Mode) a where
    Family 'Foo a = a
    Family 'Bar a = ()

data Test (m :: Mode) = Test
    { test_field1 :: Family m Int
    , test_field2 :: Family m Bool
    }

mkRecord ''Test
