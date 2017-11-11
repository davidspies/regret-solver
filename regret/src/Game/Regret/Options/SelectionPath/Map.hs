{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Game.Regret.Options.SelectionPath.Map
    ( SelectionMap
    , forMWithKey
    , fromList
    , map
    , noPaths
    , nullPaths
    , singleton
    , union
    ) where

import Data.Coerce (coerce)
import Prelude hiding (map)

import Data.Map.Generic (Key, Map)
import qualified Data.Map.Generic as Map
import Game.Regret.Options.SelectionPath

newtype SelectionMap a = SelectionMap (a (Int, Float))

map :: forall a x. (Map a, Map.MapValue a x, Map.MapValue a (Int, Float))
  => (x -> (SelectionPath, Float)) -> a x -> SelectionMap a
map = coerce $ Map.map @a @x @(Int, Float)

singleton :: forall a. (Map a, Map.MapValue a (Int, Float))
  => Key a -> (SelectionPath, Float) -> SelectionMap a
singleton = coerce $ Map.singleton @a @(Int, Float)

fromList :: forall a. (Map a, Map.MapValue a (Int, Float))
  => [(Key a, (SelectionPath, Float))] -> SelectionMap a
fromList = coerce $ Map.fromList @a @(Int, Float)

forMWithKey :: (Map.KeyTraversable a, Map.MapValue a (Int, Float), Map.MapValue a x, Monad f)
  => SelectionMap a -> (Key a -> (SelectionPath, Float) -> f x) -> f (a x)
forMWithKey (SelectionMap m) = Map.forMWithKey m . coerce

union :: forall a. (Map a, Map.MapValue a (Int, Float))
  => SelectionMap a -> SelectionMap a -> SelectionMap a
union = coerce $ Map.union @a @(Int, Float)
