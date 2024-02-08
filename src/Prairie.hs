-- | A library for first-class record fields.
--
-- @since 0.0.1.0
module Prairie
    ( module Prairie.Class
    , module Prairie.Update
    , module Prairie.Diff
    , module Prairie.Fold
    , module Prairie.Traverse
    , module Prairie.TH
    ) where

import Prairie.Class
import Prairie.Diff
import Prairie.Fold
import Prairie.TH
import Prairie.Traverse
import Prairie.Update
