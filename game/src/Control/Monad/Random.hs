{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Rank2Types #-}

module Control.Monad.Random
    ( MonadRandom(..)
    , uniformList
    , uniformListSubset
    ) where

import Control.Monad (zipWithM_)
import Control.Monad.ST (runST)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Vector as DVec
import qualified Data.Vector.Mutable as DMVec
import System.Random.PCG (Variate)

class Monad m => MonadRandom m where
  getUniform :: Variate a => m a
  getUniformR :: Variate a => (a, a) -> m a

uniformList :: MonadRandom m => NonEmpty a -> m a
uniformList xs = do
  let lenxs = length xs
  (NonEmpty.toList xs !!) <$> getUniformR (0, lenxs - 1)

uniformListSubset :: MonadRandom m => Int -> [a] -> m [a]
uniformListSubset k xs
  | k <= 0 = return []
  | k >= lenxs = return xs
  | otherwise = do
      selecteds <- mapM (\i -> getUniformR (i, lenxs - 1)) [0..(k-1)]
      return $ DVec.toList $ runST $ do
        v <- DVec.thaw (DVec.fromList xs)
        zipWithM_ (DMVec.swap v) [0..k] selecteds
        DVec.take k <$> DVec.freeze v
  where
    lenxs = length xs
