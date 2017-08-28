{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.DList.Strict
    ( DList
    , append
    , empty
    , singleton
    , snoc
    ) where

import qualified Data.DList as Lazy
import Data.Hashable (Hashable)

import Orphans ()

newtype DList a = DList (Lazy.DList a)
  deriving (Eq, Ord, Show, Hashable)

empty :: DList a
empty = DList Lazy.empty

singleton :: a -> DList a
singleton !x = DList $ Lazy.singleton x

append :: DList a -> DList a -> DList a
append (DList !x) (DList !y) = DList $ x `Lazy.append` y

snoc :: DList a -> a -> DList a
snoc (DList !xs) !x = DList $ xs `Lazy.snoc` x
