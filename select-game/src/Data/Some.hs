{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Some
    ( EqAll
    , HashableAll
    , OrdAll
    , ShowAll(..)
    , Some(..)
    , UnParam(..)
    ) where

import Data.Functor.Classes (showsUnaryWith)
import Data.Hashable (Hashable(..))

data Some a = forall x. Some !(a x)

class UnParam a where
  data RemoveParam a
  unparam :: a x -> RemoveParam a

class (UnParam a, Eq (RemoveParam a)) => EqAll a
class (UnParam a, Ord (RemoveParam a), EqAll a) => OrdAll a
class ShowAll a where
  showsPrecAll :: Int -> a x -> ShowS
class (UnParam a, Hashable (RemoveParam a)) => HashableAll a

instance EqAll a => Eq (Some a) where
  (==) (Some x) (Some y) = unparam x == unparam y
instance OrdAll a => Ord (Some a) where
  compare (Some x) (Some y) = compare (unparam x) (unparam y)
instance ShowAll a => Show (Some a) where
  showsPrec d (Some x) = showsUnaryWith showsPrecAll "Some" d x
instance HashableAll a => Hashable (Some a) where
  hashWithSalt salt (Some x) = hashWithSalt salt (unparam x)
