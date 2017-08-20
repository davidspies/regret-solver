{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Some
    ( ShowAll(..)
    , Some(..)
    , UnParam(..)
    ) where

import Data.Hashable (Hashable(..))

data Some a = forall x. Some (a x)

class UnParam a where
  data RemoveParam a
  unparam :: a x -> RemoveParam a

class ShowAll a where
  showsPrecAll :: Int -> a x -> ShowS

instance (UnParam a, Eq (RemoveParam a)) => Eq (Some a) where
  (==) (Some x) (Some y) = unparam x == unparam y
instance (UnParam a, Ord (RemoveParam a)) => Ord (Some a) where
  compare (Some x) (Some y) = compare (unparam x) (unparam y)
instance (ShowAll a) => Show (Some a) where
  showsPrec d (Some x) = showParen (d > 10) $ showString "Some " . showsPrecAll 11 x
instance (UnParam a, Hashable (RemoveParam a)) => Hashable (Some a) where
  hashWithSalt salt (Some x) = hashWithSalt salt (unparam x)
