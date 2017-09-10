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

import Control.Monad (unless)
import Control.Monad.Loops (whileM_)
import Control.Monad.ST (ST)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.STRef (modifySTRef, newSTRef, readSTRef)
import qualified Data.Vector.Storable as DVec
import qualified Data.Vector.Storable.Mutable as DMVec
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

xor :: Bool -> Bool -> Bool
xor False = id
xor True  = not

uniformListSubset :: MonadSTRandom m => Int -> [a] -> m [a]
uniformListSubset k xs = let lenxs = length xs in
  if
    | k <= 0 -> return []
    | k >= lenxs -> return xs
    | otherwise -> do
        let halflen = lenxs `quot` 2
            (inverted, count) = if k > halflen then (True, lenxs - k) else (False, k)
        selecteds <- withGen $ \gen -> do
          selected <- DMVec.replicate lenxs False
          nselected <- newSTRef (0 :: Int)
          whileM_ ((< count) <$> readSTRef nselected) $ do
            i <- uniformR (0, lenxs - 1) gen
            isRepeat <- DMVec.read selected i
            unless isRepeat $ do
              DMVec.write selected i True
              modifySTRef nselected (+ 1)
          DVec.freeze selected
        return [x | (c, x) <- zip (DVec.toList selecteds) xs, inverted `xor` c]
