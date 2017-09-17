{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Map.VecMap
    ( VecMap(..)
    ) where

import Control.Monad (forM_)
import Data.Strict.Maybe
import qualified Data.Vector as DVec
import qualified Data.Vector.Mutable as Mutable.DVec
import Prelude hiding (Maybe(..), map, maybe)
import qualified Prelude as P

import Data.Map.Generic (Key, KeyTraversable(..), Map(..), MapValue)
import Data.Strict.Maybe.Util
import Orphans ()

newtype VecMap a = VecMap {unvm :: DVec.Vector (Maybe a)}
  deriving (Foldable, Show)

instance Functor VecMap where
  fmap func (VecMap v) = VecMap $ ground $ fmap func <$> v

instance Traversable VecMap where
  traverse func (VecMap v) = VecMap . ground <$> traverse (traverse func) v

type instance Key VecMap = Int
type instance MapValue VecMap a = ()

resizedv :: DVec.Vector (Maybe a) -> Int -> DVec.Vector (Maybe a)
resizedv v n =
  case compare lv n of
    LT -> v DVec.++ DVec.replicate (n - lv) Nothing
    EQ -> v
    GT -> error "Cannot shrink vectors"
  where
    lv = DVec.length v

unifyLengths
  :: DVec.Vector (Maybe a) -> DVec.Vector (Maybe b)
  -> (DVec.Vector (Maybe a), DVec.Vector (Maybe b))
unifyLengths x y = (resizedv x lm, resizedv y lm)
  where
    lm = max (DVec.length x) (DVec.length y)

ground :: DVec.Vector (Maybe a) -> DVec.Vector (Maybe a)
ground v = DVec.foldr seq v v

instance Map VecMap where
  lookup k (VecMap v) = P.maybe P.Nothing (maybe P.Nothing P.Just) (v DVec.!? k)
  adjust func k = VecMap . DVec.modify (\vr -> do
      curVal <- Mutable.DVec.read vr k
      let newval = fmap func curVal
      newval `seq` Mutable.DVec.write vr k newval
    ) . unvm
  null = DVec.all isNothing . unvm
  (!) (VecMap v) = fromJust . (v DVec.!)
  toList = catMaybes . zipWith (fmap . (,)) [0..] . DVec.toList . unvm
  mapWithKey func = VecMap . ground . DVec.imap (fmap . func) . unvm
  size = DVec.foldl' (const . (+ 1)) 0 . unvm
  elems = catMaybes . DVec.toList . unvm
  delete k = VecMap . DVec.modify (\vr ->
      Mutable.DVec.write vr k Nothing
    ) . unvm
  unionWith func (VecMap x) (VecMap y) = VecMap $ ground $ DVec.zipWith (joinWith func) x' y'
    where
      (x', y') = unifyLengths x y
  intersectionWith func (VecMap x) (VecMap y) =
      VecMap $ ground $ DVec.zipWith func' x' y'
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
  fromListWith op kvs = VecMap $ ground $ DVec.modify (\vr ->
      forM_ kvs $ \(k,v) -> Mutable.DVec.modify vr (Just . maybe v (`op` v)) k
    ) (DVec.replicate len Nothing)
    where
      len = maximum (P.map fst kvs) + 1

instance KeyTraversable VecMap where
  traverseWithKey func =
    fmap (VecMap . ground) . DVec.imapM (\i ->
      maybe (pure Nothing) (fmap Just . func i)
    ) . unvm
