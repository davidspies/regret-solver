{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
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
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, isNothing)
import qualified Data.Vector as DVec
import qualified Data.Vector.Mutable as Mutable.DVec
import Prelude hiding (elem, lookup)

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

newtype VecMap a = VecMap {unvm :: DVec.Vector (Maybe a)}
  deriving (Functor, Foldable, Traversable, Show)

type instance Key VecMap = Int

resizedv :: DVec.Vector (Maybe a) -> Int -> DVec.Vector (Maybe a)
resizedv v n =
  case compare lv n of
    LT -> v DVec.++ DVec.replicate (n - lv) Nothing
    EQ -> v
    GT -> DVec.slice 0 n v
  where
    lv = DVec.length v

vhasKey :: Int -> DVec.Vector (Maybe a) -> Bool
vhasKey n x = maybe False isJust (x DVec.!? n)

unifyLengths ::
     DVec.Vector (Maybe a) -> DVec.Vector (Maybe b)
  -> (DVec.Vector (Maybe a), DVec.Vector (Maybe b))
unifyLengths x y = (resizedv x lm, resizedv y lm)
  where
    determineLJ :: Int -> Int
    determineLJ 0 = 0
    determineLJ n =
      if vhasKey (n - 1) x || vhasKey (n - 1) y then n else determineLJ (n - 1)
    lm = determineLJ (max (DVec.length x) (DVec.length y))

instance Map VecMap where
  lookup k (VecMap v) = fromMaybe Nothing (v DVec.!? k)
  adjust func k = VecMap . DVec.modify (\vr ->
      Mutable.DVec.modify vr (fmap func) k
    ) . unvm
  null = all isNothing . unvm
  (!) (VecMap v) = fromJust . (v DVec.!)
  toList = catMaybes . zipWith (fmap . (,)) [0..] . DVec.toList . unvm
  mapWithKey func = VecMap . DVec.imap (fmap . func) . unvm
  size = length
  elems = catMaybes . DVec.toList . unvm
  delete k = VecMap . DVec.modify (\vr ->
      Mutable.DVec.write vr k Nothing
    ) . unvm
  unionWith func (VecMap x) (VecMap y) = VecMap $ DVec.zipWith func' x' y'
    where
      (x', y') = unifyLengths x y
      func' Nothing y1          = y1
      func' x1 Nothing          = x1
      func' (Just x1) (Just y1) = Just $ func x1 y1
  intersectionWith func (VecMap x) (VecMap y) =
      VecMap $ DVec.zipWith func' x' y'
    where
      (x', y') = unifyLengths x y
      func' (Just x1) (Just y1) = Just $ func x1 y1
      func' _ _                 = Nothing
  singleton k v =
    VecMap (DVec.replicate k Nothing DVec.++ DVec.singleton (Just v))
  insertWith func k v = VecMap . DVec.modify (\vr ->
      Mutable.DVec.modify vr (Just . maybe v (func v)) k
    ) . unvm
  insert k v = VecMap . DVec.modify (\vr ->
      Mutable.DVec.write vr k (Just v)
    ) . unvm
  empty = VecMap DVec.empty
  fromListWith op kvs = VecMap $ DVec.modify (\vr ->
      forM_ kvs $ \(k,v) -> Mutable.DVec.modify vr (Just . maybe v (`op` v)) k
    ) (DVec.replicate len Nothing)
    where
      len = maximum (map fst kvs) + 1

elem :: Map m => Key m -> m a -> Bool
elem = (isJust .) . lookup
