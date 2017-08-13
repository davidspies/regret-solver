{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Some
    ( Some(..)
    , UnParam(..)
    ) where

import Data.Hashable (Hashable(..))

data Some a = forall x. Some (a x)

class UnParam a where
  data RemoveParam a
  unparam :: a x -> RemoveParam a

instance (UnParam a, Eq (RemoveParam a)) => Eq (Some a) where
  (==) (Some x) (Some y) = unparam x == unparam y
instance (UnParam a, Show (RemoveParam a)) => Show (Some a) where
  showsPrec d (Some x) = showParen (d > 10) $ showString "Some " . showsPrec 11 (unparam x)
instance (UnParam a, Hashable (RemoveParam a)) => Hashable (Some a) where
  hashWithSalt salt (Some x) = hashWithSalt salt (unparam x)
