{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module Game.Regret.Options.Class
    ( RegretOptions(..)
    ) where

import GHC.Exts (Constraint)

import Data.Dist (Dist, Probability)
import Data.Vector.Class (Vector)

class RegretOptions opts where
  type MConstraints opts :: ((* -> *) -> Constraint)
  selectChance :: (MConstraints opts m, Vector v) => opts -> Dist a -> (opts -> a -> m v) -> m v
  selectOpposite :: (MConstraints opts m, Vector v) => opts -> Dist a -> (opts -> a -> m v) -> m v
  selectSelf :: (MConstraints opts m, Vector v)
    => opts
    -> Dist a -- player choices
    -> (opts -> a -> m v) -- visited handling
    -> (a -> m v) -- unvisited handling
    -> m ([(Probability, a)], v)
