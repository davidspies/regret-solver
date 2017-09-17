{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Map.Generic
    ( Key
    , KeyTraversable(..)
    , Map(..)
    , MapValue
    , elem
    , forMWithKey
    ) where

import qualified Data.IntMap.Strict as StrictIntMap
import Data.List (foldl')
import qualified Data.Map.Strict as StrictMap
import Data.Maybe (fromJust, isJust)
import GHC.Exts (Constraint)
import Prelude hiding (elem, lookup)
import qualified Prelude as P

import Orphans ()

type family Key (m :: * -> *) :: *

type family MapValue (m :: * -> *) a :: Constraint

class Map (m :: * -> *) where
  lookup :: MapValue m a => Key m -> m a -> Maybe a
  adjust :: MapValue m a => (a -> a) -> Key m -> m a -> m a
  delete :: MapValue m a => Key m -> m a -> m a
  null :: MapValue m a => m a -> Bool
  null = (== 0) . size
  (!) :: MapValue m a => m a -> Key m -> a
  (!) = (fromJust .) . flip lookup
  mapWithKey :: (MapValue m a, MapValue m b) => (Key m -> a -> b) -> m a -> m b
  map :: (MapValue m a, MapValue m b) => (a -> b) -> m a -> m b
  map func = mapWithKey (const func)
  size :: MapValue m a => m a -> Int
  toList :: MapValue m a => m a -> [(Key m, a)]
  keys :: MapValue m a => m a -> [Key m]
  keys = P.map fst . toList
  elems :: MapValue m a => m a -> [a]
  elems = P.map snd . toList
  unionWith :: MapValue m a => (a -> a -> a) -> m a -> m a -> m a
  union :: MapValue m a => m a -> m a -> m a
  union = unionWith const
  intersectionWith :: (MapValue m a, MapValue m b, MapValue m c)
    => (a -> b -> c) -> m a -> m b -> m c
  intersection :: (MapValue m a, MapValue m b) => m a -> m b -> m a
  intersection = intersectionWith const
  singleton :: MapValue m a => Key m -> a -> m a
  singleton k v = fromList [(k, v)]
  insertWith :: MapValue m a => (a -> a -> a) -> Key m -> a -> m a -> m a
  insert :: MapValue m a => Key m -> a -> m a -> m a
  insert = insertWith const
  empty :: MapValue m a => m a
  empty = fromList []
  unionsWith :: MapValue m a => (a -> a -> a) -> [m a] -> m a
  unionsWith func = foldl' (unionWith func) empty
  unions :: MapValue m a => [m a] -> m a
  unions = unionsWith const
  fromListWith :: MapValue m a => (a -> a -> a) -> [(Key m, a)] -> m a
  fromList :: MapValue m a => [(Key m, a)] -> m a
  fromList = fromListWith (const id)

class Map m => KeyTraversable m where
  -- Should be Applicative, but Data.Vector.Unboxed has no itraverse
  traverseWithKey :: (Monad f, MapValue m a, MapValue m b)
    => (Key m -> a -> f b) -> m a -> f (m b)

type instance Key (StrictMap.Map k) = k
type instance MapValue (StrictMap.Map k) a = ()

instance Ord k => Map (StrictMap.Map k) where
  lookup = StrictMap.lookup
  adjust = StrictMap.adjust
  null = StrictMap.null
  (!) = (StrictMap.!)
  toList = StrictMap.toList
  mapWithKey = StrictMap.mapWithKey
  size = StrictMap.size
  keys = StrictMap.keys
  elems = StrictMap.elems
  delete = StrictMap.delete
  unionWith = StrictMap.unionWith
  union = StrictMap.union
  intersectionWith = StrictMap.intersectionWith
  intersection = StrictMap.intersection
  singleton = StrictMap.singleton
  insertWith = StrictMap.insertWith
  insert = StrictMap.insert
  empty = StrictMap.empty
  unionsWith = StrictMap.unionsWith
  unions = StrictMap.unions
  fromListWith = StrictMap.fromListWith
  fromList = StrictMap.fromList

instance Ord k => KeyTraversable (StrictMap.Map k) where
  traverseWithKey = StrictMap.traverseWithKey

type instance Key StrictIntMap.IntMap = Int
type instance MapValue StrictIntMap.IntMap a = ()

instance Map StrictIntMap.IntMap where
  lookup = StrictIntMap.lookup
  adjust = StrictIntMap.adjust
  null = StrictIntMap.null
  (!) = (StrictIntMap.!)
  toList = StrictIntMap.toList
  mapWithKey = StrictIntMap.mapWithKey
  size = StrictIntMap.size
  keys = StrictIntMap.keys
  elems = StrictIntMap.elems
  delete = StrictIntMap.delete
  unionWith = StrictIntMap.unionWith
  union = StrictIntMap.union
  intersectionWith = StrictIntMap.intersectionWith
  intersection = StrictIntMap.intersection
  singleton = StrictIntMap.singleton
  insertWith = StrictIntMap.insertWith
  insert = StrictIntMap.insert
  empty = StrictIntMap.empty
  unionsWith = StrictIntMap.unionsWith
  unions = StrictIntMap.unions
  fromListWith = StrictIntMap.fromListWith
  fromList = StrictIntMap.fromList

instance KeyTraversable StrictIntMap.IntMap where
  traverseWithKey = StrictIntMap.traverseWithKey

elem :: (Map m, MapValue m a) => Key m -> m a -> Bool
elem = (isJust .) . lookup

forMWithKey :: (Monad f, KeyTraversable m, MapValue m a, MapValue m b)
  => m a -> (Key m -> a -> f b) -> f (m b)
forMWithKey = flip traverseWithKey
