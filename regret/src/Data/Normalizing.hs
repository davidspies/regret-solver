{-# LANGUAGE TypeFamilies #-}

module Data.Normalizing
    ( Normalizing (..)
    ) where

class Normalizing a where
  type Normal a
  normalize :: a -> Normal a
  forget :: Normal a -> a
  untypedNormalize :: a -> a
  untypedNormalize = forget . normalize
