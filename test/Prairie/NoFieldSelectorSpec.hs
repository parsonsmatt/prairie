{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

#if MIN_VERSION_base(4,16,0)
{-# LANGUAGE NoFieldSelectors #-}
#endif

module Prairie.NoFieldSelectorSpec where

import Prairie

data User = User {name :: String}

mkRecord ''User

data Dog = Dog {name :: String}

mkRecord ''Dog
