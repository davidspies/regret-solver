{-# LANGUAGE TupleSections #-}

module Control.Monad.Random
    ( MonadRandom(..)
    , uniformList
    ) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Vector as DVec
import System.Random.MWC (Variate)

class Monad m => MonadRandom m where
  getUniform :: Variate a => m a
  getUniformR :: Variate a => (a, a) -> m a

uniformList :: MonadRandom m => NonEmpty a -> m a
uniformList xs = do
  let tab = DVec.fromList $ NonEmpty.toList xs
  (tab DVec.!) <$> getUniformR (0, DVec.length tab - 1)
