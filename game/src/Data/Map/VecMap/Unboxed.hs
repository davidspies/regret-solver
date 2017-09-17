{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Map.VecMap.Unboxed
    ( VecMap(..)
    ) where

import Control.Monad (forM_)
import Data.Default (Default)
import Data.Maybe (catMaybes, fromJust, isNothing)
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as DVec
import qualified Data.Vector.Unboxed.Mutable as Mutable.DVec
import Prelude hiding (map)
import qualified Prelude as P

import Data.Map.Generic (Key, KeyTraversable(..), Map(..), MapValue)
import Data.Maybe.Util (joinWith)
import Orphans ()

newtype VecMap a = VecMap {unvm :: DVec.Vector (Maybe a)}
deriving instance (Show a, Default a, Unbox a) => Show (VecMap a)

type instance Key VecMap = Int
type instance MapValue VecMap a = (Default a, Unbox a)

resizedv :: (Default a, Unbox a) => DVec.Vector (Maybe a) -> Int -> DVec.Vector (Maybe a)
resizedv v n =
  case compare lv n of
    LT -> v DVec.++ DVec.replicate (n - lv) Nothing
    EQ -> v
    GT -> error "Cannot shrink vectors"
  where
    lv = DVec.length v

unifyLengths :: (Default a, Unbox a, Default b, Unbox b)
  => DVec.Vector (Maybe a) -> DVec.Vector (Maybe b)
  -> (DVec.Vector (Maybe a), DVec.Vector (Maybe b))
unifyLengths x y = (resizedv x lm, resizedv y lm)
  where
    lm = max (DVec.length x) (DVec.length y)

instance Map VecMap where
  lookup k (VecMap v) = maybe Nothing (maybe Nothing Just) (v DVec.!? k)
  adjust func k = VecMap . DVec.modify (\vr -> do
      curVal <- Mutable.DVec.read vr k
      let newval = fmap func curVal
      newval `seq` Mutable.DVec.write vr k newval
    ) . unvm
  null = DVec.all isNothing . unvm
  (!) (VecMap v) = fromJust . (v DVec.!)
  toList = catMaybes . zipWith (fmap . (,)) [0..] . DVec.toList . unvm
  mapWithKey func = VecMap . DVec.imap (fmap . func) . unvm
  size = DVec.foldl' (const . (+ 1)) 0 . unvm
  elems = catMaybes . DVec.toList . unvm
  delete k = VecMap . DVec.modify (\vr ->
      Mutable.DVec.write vr k Nothing
    ) . unvm
  unionWith func (VecMap x) (VecMap y) = VecMap $ DVec.zipWith (joinWith func) x' y'
    where
      (x', y') = unifyLengths x y
  intersectionWith func (VecMap x) (VecMap y) =
      VecMap $ DVec.zipWith func' x' y'
    where
      (x', y') = unifyLengths x y
      func' (Just x1) (Just y1) = Just $ func x1 y1
      func' _ _                 = Nothing
  singleton k !v =
    VecMap (DVec.replicate k Nothing DVec.++ DVec.singleton (Just v))
  insertWith func k v = VecMap . DVec.modify (\vr -> do
      curVal <- Mutable.DVec.read vr k
      let newval = Just $ maybe v (func v) curVal
      newval `seq` Mutable.DVec.write vr k newval
    ) . unvm
  insert k !v = VecMap . DVec.modify (\vr ->
      Mutable.DVec.write vr k (Just v)
    ) . unvm
  empty = VecMap DVec.empty
  fromListWith op kvs = VecMap $ DVec.modify (\vr ->
      forM_ kvs $ \(k,v) -> Mutable.DVec.modify vr (Just . maybe v (`op` v)) k
    ) (DVec.replicate len Nothing)
    where
      len = maximum (P.map fst kvs) + 1

instance KeyTraversable VecMap where
  traverseWithKey func =
    fmap VecMap . DVec.imapM (\i ->
      maybe (pure Nothing) (fmap Just . func i)
    ) . unvm
