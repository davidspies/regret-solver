module Data.Vector.Class
    ( Vector (..)
    , genericScaleMap
    , genericAddMap
    , genericVNegateMap
    , genericZeroMap
    , genericVSumMap
    , genericScaleApplicative
    , genericAddApplicative
    , genericVNegateApplicative
    , genericSubApplicative
    , genericZeroApplicative
    , genericVSumApplicative
    ) where

import Control.Applicative (liftA2)
import Data.IntMap.Strict (IntMap)
import Data.List (foldl')
import qualified Data.Map.Strict as StrictMap
import Data.Maybe (fromJust, fromMaybe, isJust)
import qualified Data.Vector as DVec

import Data.Map.Generic (Map)
import qualified Data.Map.Generic as Map

class Vector v where
  scale :: Float -> v -> v
  add :: v -> v -> v
  vnegate :: v -> v
  vnegate = scale (-1)
  sub :: v -> v -> v
  sub x y = add x (vnegate y)
  zero :: v
  vsum :: [v] -> v
  vsum = foldl' add zero

zipOuterWith :: (a -> b -> c) -> a -> b -> [a] -> [b] -> [c]
zipOuterWith op defx defy xs ys =
    map fromJust $ takeWhile isJust $ zipWith helper (infinify xs) (infinify ys)
  where
    infinify xs' = map Just xs' ++ repeat Nothing
    helper Nothing Nothing = Nothing
    helper x y             = Just $ fromMaybe defx x `op` fromMaybe defy y

instance Vector a => Vector [a] where
  scale c = map (scale c)
  add = zipOuterWith add zero zero
  vnegate = map vnegate
  sub = zipOuterWith sub zero zero
  zero = []

instance Vector b => Vector (a -> b) where
  scale c = (scale c .)
  add x y z = add (x z) (y z)
  vnegate = (vnegate .)
  sub x y z = sub (x z) (y z)
  zero = const zero
  vsum xs z = vsum $ map ($ z) xs

instance Vector Float where
  scale = (*)
  add = (+)
  vnegate = negate
  sub = (-)
  zero = 0
  vsum = sum

instance (Vector a, Vector b) => Vector (a, b) where
  scale c (x, y) = (scale c x, scale c y)
  add (x1, y1) (x2, y2) = (add x1 x2, add y1 y2)
  vnegate (x, y) = (vnegate x, vnegate y)
  sub (x1, y1) (x2, y2) = (sub x1 x2, sub y1 y2)
  zero = (zero, zero)
  vsum prs = let (xs, ys) = unzip prs in (vsum xs, vsum ys)

genericScaleFunctor :: (Functor m, Vector x) => Float -> m x -> m x
genericScaleFunctor = fmap . scale
genericVNegateFunctor :: (Functor m, Vector x) => m x -> m x
genericVNegateFunctor = fmap vnegate

genericScaleMap :: (Map m, Vector x) => Float -> m x -> m x
genericScaleMap = genericScaleFunctor
genericAddMap :: (Map m, Vector x) => m x -> m x -> m x
genericAddMap = Map.unionWith add
genericVNegateMap :: (Map m, Vector x) => m x -> m x
genericVNegateMap = genericVNegateFunctor
genericZeroMap :: (Map m) => m x
genericZeroMap = Map.empty
genericVSumMap :: (Map m, Vector x) => [m x] -> m x
genericVSumMap = Map.unionsWith add

instance (Ord k, Vector a) => Vector (StrictMap.Map k a) where
  scale = genericScaleMap
  add = genericAddMap
  vnegate = genericVNegateMap
  zero = genericZeroMap
  vsum = genericVSumMap

instance Vector a => Vector (IntMap a) where
  scale = genericScaleMap
  add = genericAddMap
  vnegate = genericVNegateMap
  zero = genericZeroMap
  vsum = genericVSumMap

instance Vector a => Vector (Map.VecMap a) where
  scale = genericScaleMap
  add = genericAddMap
  vnegate = genericVNegateMap
  zero = genericZeroMap
  vsum = genericVSumMap

genericScaleApplicative :: (Applicative m, Vector x) => Float -> m x -> m x
genericScaleApplicative = genericScaleFunctor
genericAddApplicative :: (Applicative a, Vector x) => a x -> a x -> a x
genericAddApplicative = liftA2 add
genericVNegateApplicative :: (Applicative m, Vector x) => m x -> m x
genericVNegateApplicative = genericVNegateFunctor
genericSubApplicative :: (Applicative a, Vector x) => a x -> a x -> a x
genericSubApplicative = liftA2 sub
genericZeroApplicative :: (Applicative a, Vector x) => a x
genericZeroApplicative = pure zero
genericVSumApplicative :: (Applicative a, Vector x) => [a x] -> a x
genericVSumApplicative = fmap vsum . sequenceA

instance Vector a => Vector (Maybe a) where
  scale = genericScaleApplicative
  add = genericAddApplicative
  vnegate = genericVNegateApplicative
  sub = genericSubApplicative
  zero = genericZeroApplicative
  vsum = genericVSumApplicative

instance Vector a => Vector (DVec.Vector a) where
  scale = DVec.map . scale
  add x y = DVec.zipWith add (pad zero mlen x) (pad zero mlen y)
    where
      mlen = max (DVec.length x) (DVec.length y)
      pad z len v
        | lenv >= len = v
        | otherwise = v DVec.++ DVec.replicate (len - lenv) z
        where
          lenv = DVec.length v
  vnegate = DVec.map vnegate
  zero = DVec.empty
