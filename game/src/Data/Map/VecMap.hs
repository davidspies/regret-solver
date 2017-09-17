{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Map.VecMap
    ( VecMap(..)
    ) where

import Control.Monad (forM_)
import qualified Data.Strict as Strict
import qualified Data.Strict.Maybe.Util as Strict
import qualified Data.Vector as DVec
import qualified Data.Vector.Mutable as Mutable.DVec

import Data.Map.Generic (Key, Map(..))

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
    GT -> error "Cannot shrink vectors"
  where
    lv = DVec.length v

unifyLengths :: DVec.Vector (Strict.Maybe a) -> DVec.Vector (Strict.Maybe b)
  -> (DVec.Vector (Strict.Maybe a), DVec.Vector (Strict.Maybe b))
unifyLengths x y = (resizedv x lm, resizedv y lm)
  where
    lm = max (DVec.length x) (DVec.length y)

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
  toList = Strict.catMaybes . zipWith (fmap . (,)) [0..] . DVec.toList . unvm
  mapWithKey func = VecMap . ground . DVec.imap (fmap . func) . unvm
  size = length
  elems = Strict.catMaybes . DVec.toList . unvm
  delete k = VecMap . DVec.modify (\vr ->
      Mutable.DVec.write vr k Strict.Nothing
    ) . unvm
  unionWith func (VecMap x) (VecMap y) = VecMap $ ground $ DVec.zipWith (Strict.joinWith func) x' y'
    where
      (x', y') = unifyLengths x y
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
