module Prairie.Internal where

import Control.Monad.Reader (MonadReader (..), asks)
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))

type Getting r s a = (a -> Const r a) -> s -> Const r s

type ASetter s t a b = (a -> Identity b) -> s -> Identity t

type Lens s t a b = forall f. (Functor f) => (a -> f b) -> s -> f t

type Lens' s a = Lens s s a a

set :: ASetter s t a b -> b -> s -> t
set l b = runIdentity . l (\_ -> Identity b)
{-# INLINE set #-}

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = sbt s <$> afb (sa s)
{-# INLINE lens #-}

view :: (MonadReader s m) => Getting a s a -> m a
view l = asks (getConst . l Const)
{-# INLINE view #-}
