{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}

module Control.Monad.Random
    ( MonadRandom(..)
    , MonadSTRandom(..)
    , uniformList
    , uniformListSubset
    , stGetUniform
    , stGetUniformR
    ) where

import Control.Monad (forM_, zipWithM)
import Control.Monad.ST (ST)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Vector as DVec
import qualified Data.Vector.Mutable as DMVec
import System.Random.PCG (GenST, Variate, uniform, uniformR)

class Monad m => MonadRandom m where
  getUniform :: Variate a => m a
  getUniformR :: Variate a => (a, a) -> m a

stGetUniform :: (MonadSTRandom m, Variate a) => m a
stGetUniform = withGen uniform

stGetUniformR :: (MonadSTRandom m, Variate a) => (a, a) -> m a
stGetUniformR bnds = withGen (uniformR bnds)

class MonadRandom m => MonadSTRandom m where
  withGen :: (forall s. GenST s -> ST s a) -> m a

uniformList :: MonadRandom m => NonEmpty a -> (Bool -> a -> m b) -> m (a, NonEmpty b)
uniformList xs f = do
  let nxs = NonEmpty.toList xs
      lenxs = length nxs
  choice <- getUniformR (0, lenxs - 1)
  res <- zipWithM (\i -> f (i == choice)) [0..] nxs
  return (nxs !! choice, NonEmpty.fromList res)

uniformListSubset :: MonadSTRandom m => Int -> [a] -> (Bool -> a -> m b) -> m ([a], [b])
uniformListSubset k xs f
  | k <= 0 = ([],) <$> mapM (f False) xs
  | null $ drop k xs = (xs,) <$> mapM (f True) xs
  | otherwise = do
      chosen <- fmap DVec.toList $ withGen $ \gen -> do
        let lenxs = length xs
        v <- DVec.thaw $ DVec.replicate k True DVec.++ DVec.replicate (lenxs - k) False
        forM_ [0..(lenxs - 1)] $ \i -> DMVec.swap v i =<< uniformR (i, lenxs - 1) gen
        DVec.freeze v
      res <- zipWithM f chosen xs
      return (map snd $ filter fst $ zip chosen xs, res)
