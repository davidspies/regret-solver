{-# LANGUAGE TypeFamilies #-}

module Data.Map.Generic
    ( Key
    , Map(..)
    , elem
    ) where

import qualified Data.IntMap.Strict as StrictIntMap
import Data.List (foldl')
import qualified Data.Map.Strict as StrictMap
import Data.Maybe (fromJust, isJust)
import Prelude hiding (elem, lookup)

import Orphans ()

type family Key (m :: * -> *) :: *

class Traversable m => Map (m :: * -> *) where
  lookup :: Key m -> m a -> Maybe a
  adjust :: (a -> a) -> Key m -> m a -> m a
  delete :: Key m -> m a -> m a
  null :: m a -> Bool
  null = (== 0) . size
  (!) :: m a -> Key m -> a
  (!) = (fromJust .) . flip lookup
  mapWithKey :: (Key m -> a -> b) -> m a -> m b
  size :: m a -> Int
  toList :: m a -> [(Key m, a)]
  keys :: m a -> [Key m]
  keys = map fst . toList
  elems :: m a -> [a]
  elems = map snd . toList
  unionWith :: (a -> a -> a) -> m a -> m a -> m a
  union :: m a -> m a -> m a
  union = unionWith const
  intersectionWith :: (a -> b -> c) -> m a -> m b -> m c
  intersection :: m a -> m b -> m a
  intersection = intersectionWith const
  singleton :: Key m -> a -> m a
  singleton k v = fromList [(k, v)]
  insertWith :: (a -> a -> a) -> Key m -> a -> m a -> m a
  insert :: Key m -> a -> m a -> m a
  insert = insertWith const
  empty :: m a
  empty = fromList []
  unionsWith :: (a -> a -> a) -> [m a] -> m a
  unionsWith func = foldl' (unionWith func) empty
  unions :: [m a] -> m a
  unions = unionsWith const
  fromListWith :: (a -> a -> a) -> [(Key m, a)] -> m a
  fromList :: [(Key m, a)] -> m a
  fromList = fromListWith (const id)

type instance Key (StrictMap.Map k) = k

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

type instance Key StrictIntMap.IntMap = Int

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

elem :: Map m => Key m -> m a -> Bool
elem = (isJust .) . lookup
