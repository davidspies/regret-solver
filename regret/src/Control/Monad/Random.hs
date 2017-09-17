{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Rank2Types #-}

module Control.Monad.Random
    ( MonadRandom(..)
    , MonadSTRandom(..)
    , uniformList
    , uniformListSubset
    , stGetUniform
    , stGetUniformR
    ) where

import Control.Monad (forM_)
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

uniformList :: MonadRandom m => NonEmpty a -> m a
uniformList xs = do
  let lenxs = length xs
  (NonEmpty.toList xs !!) <$> getUniformR (0, lenxs - 1)

uniformListSubset :: MonadSTRandom m => Int -> [a] -> m [a]
uniformListSubset k xs
  | k <= 0 = return []
  | null $ drop k xs = return xs
  | otherwise = fmap DVec.toList $ withGen $ \gen -> do
      let vxs = DVec.fromList xs
          lenxs = DVec.length vxs
      v <- DVec.thaw vxs
      forM_ [0..(k - 1)] $ \i -> DMVec.swap v i =<< uniformR (i, lenxs - 1) gen
      DVec.take k <$> DVec.freeze v
