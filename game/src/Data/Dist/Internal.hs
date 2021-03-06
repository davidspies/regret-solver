{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Dist.Internal
    ( SDist
    , expected
    , extractSingleton
    , nonZeroPieces
    , normalize
    , sample
    , singleton
    , withProbability
    ) where

import Control.Arrow (first)
import Control.Monad.Random (Randomizable, getUniform)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromJust)

import Data.Vector.Class (Vector)
import qualified Data.Vector.Class as Vector

newtype SDist a = SDist { pieces :: NonEmpty (Float, a) }
  deriving (Functor, Show)

singleton :: a -> SDist a
singleton x = SDist $ (1, x) :| []

extractSingleton :: SDist a -> Maybe a
extractSingleton (SDist ((_, x) :| [])) = Just x
extractSingleton _                      = Nothing

normalize :: NonEmpty (Float, a) -> SDist a
normalize xs
  | total <= 0 = SDist $ NonEmpty.map (first (const uniProb)) posified
  | otherwise  = SDist $ NonEmpty.map (\(d, x) -> (d / total, x)) posified
  where
    posified = NonEmpty.map (first (max 0)) xs
    total = sum $ NonEmpty.map fst posified
    uniProb :: Float
    uniProb = 1 / fromIntegral (length xs)

expected :: (Vector a) => SDist a -> a
expected (SDist xs) = Vector.vsum [Vector.scale d x | (d, x) <- NonEmpty.toList xs, d /= 0]

accumsOf :: SDist a -> NonEmpty (Float, a)
accumsOf (SDist xs) = NonEmpty.scanl1 (\(cur, _) (d, y) -> (cur + d, y)) xs

sample :: Randomizable m Float => SDist a -> m a
sample ds = do
  let accumed = accumsOf ds
  d <- getUniform
  return $ snd $ fromJust $ find ((>= d) . fst) accumed

withProbability :: SDist a -> SDist (Float, a)
withProbability (SDist xs) = SDist $ NonEmpty.map (\(d, x) -> (d, (d, x))) xs

nonZeroPieces :: SDist a -> NonEmpty (Float, a)
nonZeroPieces = NonEmpty.fromList . filter ((/= 0) . fst) . NonEmpty.toList . pieces
