{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Map.Generic
    ( Key
    , Map(..)
    , VecMap(..)
    , elem
    ) where

import Control.Monad (forM_)
import qualified Data.IntMap.Strict as StrictIntMap
import Data.List (foldl')
import qualified Data.Map.Strict as StrictMap
import Data.Maybe (fromJust, isJust, mapMaybe)
import qualified Data.Strict.Maybe as Strict
import qualified Data.Vector as DVec
import qualified Data.Vector.Mutable as Mutable.DVec
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

newtype VecMap a = VecMap {unvm :: DVec.Vector (Strict.Maybe a)}
  deriving (Foldable, Show)

instance Functor VecMap where
  fmap func (VecMap v) = VecMap $ ground $ fmap func <$> v

instance Traversable VecMap where
  traverse func (VecMap v) = VecMap . ground <$> traverse (traverse func) v

type instance Key VecMap = Int

resizedv :: DVec.Vector (Strict.Maybe a) -> Int -> DVec.Vector (Strict.Maybe a)
resizedv v n =
  case compare lv n of
    LT -> v DVec.++ DVec.replicate (n - lv) Strict.Nothing
    EQ -> v
    GT -> DVec.slice 0 n v
  where
    lv = DVec.length v

vhasKey :: Int -> DVec.Vector (Strict.Maybe a) -> Bool
vhasKey n x = maybe False Strict.isJust (x DVec.!? n)

unifyLengths :: DVec.Vector (Strict.Maybe a) -> DVec.Vector (Strict.Maybe b)
  -> (DVec.Vector (Strict.Maybe a), DVec.Vector (Strict.Maybe b))
unifyLengths x y = (resizedv x lm, resizedv y lm)
  where
    determineLJ :: Int -> Int
    determineLJ 0 = 0
    determineLJ n = if vhasKey (n - 1) x || vhasKey (n - 1) y then n else determineLJ (n - 1)
    lm = determineLJ (max (DVec.length x) (DVec.length y))

strictCatMaybes :: [Strict.Maybe a] -> [a]
strictCatMaybes = mapMaybe (Strict.maybe Nothing Just)

ground :: DVec.Vector (Strict.Maybe a) -> DVec.Vector (Strict.Maybe a)
ground v = foldr seq v v

instance Map VecMap where
  lookup k (VecMap v) = maybe Nothing (Strict.maybe Nothing Just) (v DVec.!? k)
  adjust func k = VecMap . DVec.modify (\vr -> do
      curVal <- Mutable.DVec.read vr k
      let newval = fmap func curVal
      newval `seq` Mutable.DVec.write vr k newval
    ) . unvm
  null = all Strict.isNothing . unvm
  (!) (VecMap v) = Strict.fromJust . (v DVec.!)
  toList = strictCatMaybes . zipWith (fmap . (,)) [0..] . DVec.toList . unvm
  mapWithKey func = VecMap . ground . DVec.imap (fmap . func) . unvm
  size = length
  elems = strictCatMaybes . DVec.toList . unvm
  delete k = VecMap . DVec.modify (\vr ->
      Mutable.DVec.write vr k Strict.Nothing
    ) . unvm
  unionWith func (VecMap x) (VecMap y) = VecMap $ ground $ DVec.zipWith func' x' y'
    where
      (x', y') = unifyLengths x y
      func' Strict.Nothing y1                 = y1
      func' x1 Strict.Nothing                 = x1
      func' (Strict.Just x1) (Strict.Just y1) = Strict.Just $ func x1 y1
  intersectionWith func (VecMap x) (VecMap y) =
      VecMap $ ground $ DVec.zipWith func' x' y'
    where
      (x', y') = unifyLengths x y
      func' (Strict.Just x1) (Strict.Just y1) = Strict.Just $ func x1 y1
      func' _ _                               = Strict.Nothing
  singleton k !v =
    VecMap (DVec.replicate k Strict.Nothing DVec.++ DVec.singleton (Strict.Just v))
  insertWith func k v = VecMap . DVec.modify (\vr -> do
      curVal <- Mutable.DVec.read vr k
      let newval = Strict.Just $ Strict.maybe v (func v) curVal
      newval `seq` Mutable.DVec.write vr k newval
    ) . unvm
  insert k !v = VecMap . DVec.modify (\vr ->
      Mutable.DVec.write vr k (Strict.Just v)
    ) . unvm
  empty = VecMap DVec.empty
  fromListWith op kvs = VecMap $ ground $ DVec.modify (\vr ->
      forM_ kvs $ \(k,v) -> Mutable.DVec.modify vr (Strict.Just . Strict.maybe v (`op` v)) k
    ) (DVec.replicate len Strict.Nothing)
    where
      len = maximum (map fst kvs) + 1

elem :: Map m => Key m -> m a -> Bool
elem = (isJust .) . lookup
