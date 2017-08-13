{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Scale
    ( MonadScale (..)
    , ScaleT
    , Scale
    ) where

import Control.Monad.Reader (ReaderT, ask, withReaderT)
import Data.Functor.Identity (Identity)

class Monad m => MonadScale m where
  scaleBy :: Double -> m a -> m a
  coefficient :: m Double

newtype ScaleT m a = ScaleT (ReaderT Double m a)
  deriving (Functor, Applicative, Monad)

instance Monad m => MonadScale (ScaleT m) where
  scaleBy c (ScaleT act) = ScaleT $ withReaderT (c *) act
  coefficient = ScaleT ask

type Scale = ScaleT Identity
