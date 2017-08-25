{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}

module Game.Regret.Monad
    ( MonadRandom
    , Regret
    , TopRegret
    , averageValue
    , addRegret
    , getUniformR
    , regretValue
    , runRegret
    , saveRegrets_
    ) where

import Control.Monad (ap, forM_, void, when)
import Control.Monad.Random (MonadRandom(..))
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT, withReaderT)
import Control.Monad.ST (ST, runST)
import Data.List (sortOn)
import Data.Maybe (isNothing)
import Data.STRef (STRef, modifySTRef, newSTRef, readSTRef, writeSTRef)
import System.Random.MWC (GenST)
import qualified System.Random.MWC as MWC

import Control.Monad.Scale
import Data.Map.Mutable.Generic as Mutable (Map)
import Data.Map.Mutable.Generic as Mutable.Map
import Data.Normalizing
import Data.Vector.Class

data Env m s v = Env
  { tenv        :: TopEnv m s v
  , useMap      :: m s ()
  , used        :: STRef s [Key m]
  , scaleFactor :: Double
  }

data AccumNode v = AccumNode
  { accumValue :: v
  , prevValue  :: v
  , visits     :: Int
  , lastUpdate :: Int
  }

newAccum :: Vector v => Int -> v -> AccumNode v
newAccum current value =
  AccumNode{accumValue = zero, prevValue = value, visits = 1, lastUpdate = current}

update :: (Normalizing v, Vector v) => Int -> AccumNode v -> AccumNode v
update current AccumNode{..} = AccumNode
  { accumValue =
      accumValue `add` scale (fromIntegral (current - lastUpdate)) (untypedNormalize prevValue)
  , prevValue
  , visits
  , lastUpdate = current
  }

joinAccum :: (Normalizing v, Vector v) => AccumNode v -> AccumNode v -> AccumNode v
joinAccum x y = AccumNode
  { accumValue = accumValue x' `add` accumValue y'
  , prevValue = recentValue
  , visits = visits x + visits y
  , lastUpdate = recentUpdate
  }
  where
    recentUpdate = max (lastUpdate x) (lastUpdate y)
    x' = update recentUpdate x
    y' = update recentUpdate y
    recentValue = if lastUpdate y > lastUpdate x then prevValue y else prevValue x

data TopEnv m s v = TopEnv
  { regretMap  :: m s v
  , accumMap   :: m s (AccumNode v)
  , randSource :: GenST s
  , iteration  :: STRef s Int
  }

instance Applicative (Regret m v) where
  pure = return
  (<*>) = ap

instance Monad (Regret m v) where
  return x = Regret $ return x
  (>>=) (Regret x) f = Regret $ x >>= f'
    where
      f' x' = let Regret z = f x' in z

instance Applicative (TopRegret m v) where
  pure = return
  (<*>) = ap

instance Monad (TopRegret m v) where
  return x = TopRegret $ return x
  (>>=) (TopRegret x) f = TopRegret $ x >>= f'
    where
      f' x' = let TopRegret z = f x' in z

instance MonadScale (Regret m v) where
  scaleBy c (Regret v) =
    Regret $
    withReaderT (\e@Env{scaleFactor} -> e{scaleFactor = c * scaleFactor}) v
  coefficient = Regret $ scaleFactor <$> ask

randOp :: (forall s. GenST s -> ST s a) -> Regret m v a
randOp srcOp = Regret $ do
  Env{tenv = TopEnv{randSource}} <- ask
  liftST $ srcOp randSource

instance MonadRandom (Regret m v) where
  getUniform = randOp MWC.uniform
  getUniformR bnds = randOp (MWC.uniformR bnds)

newtype TopRegret m v a = TopRegret (forall s. ReaderT (TopEnv m s v) (ST s) a)
  deriving (Functor)

newtype Regret m v a = Regret (forall s. ReaderT (Env m s v) (ST s) a)
  deriving (Functor)

liftST :: ST s a -> ReaderT (Env m s v) (ST s) a
liftST = lift

liftSTTop :: ST s a -> ReaderT (TopEnv m s v) (ST s) a
liftSTTop = lift

addRegret :: (Mutable.Map m, Vector v) => Key m -> v -> Regret m v ()
addRegret k v = Regret $ do
  Env{tenv = TopEnv{regretMap}, used, useMap, scaleFactor} <- ask
  liftST $ do
    Mutable.Map.insertWith add k (scale scaleFactor v) regretMap
    present <- Mutable.Map.lookup k useMap
    when (isNothing present) $ do
      Mutable.Map.insert k () useMap
      modifySTRef used (k :)

saveRegrets_ :: (Mutable.Map m, Vector v, Normalizing v)
  => Regret m v a -> TopRegret m v ()
saveRegrets_ (Regret r) = TopRegret $ do
  useMap <- liftSTTop Mutable.Map.new
  used <- liftSTTop $ newSTRef []
  void $ withReaderT (\x -> Env{tenv = x, useMap, used, scaleFactor = 1}) r
  TopEnv{regretMap, accumMap, iteration} <- ask
  liftSTTop $ do
    usedV <- readSTRef used
    current <- (+ 1) <$> readSTRef iteration
    forM_ usedV $ \k -> do
      Just val <- Mutable.Map.lookup k regretMap
      Mutable.Map.insertWith joinAccum k (newAccum current val) accumMap
    writeSTRef iteration current

regretValue :: (Normalizing v, Mutable.Map m) => Key m -> Regret m v (Maybe (Normal v))
regretValue k = Regret $ do
  Env{tenv = TopEnv{regretMap}} <- ask
  liftST $ fmap normalize <$> Mutable.Map.lookup k regretMap

averageValue :: (Normalizing v, Mutable.Map m) => Key m -> Regret m v (Maybe (Normal v))
averageValue k = Regret $ do
  Env{tenv = TopEnv{accumMap}} <- ask
  liftST $ fmap (normalize . accumValue) <$> Mutable.Map.lookup k accumMap

runRegret :: (Mutable.Map m, Ord (Key m), Normalizing v, Vector v)
  => MWC.Seed -> TopRegret m v () -> [(Key m, v, Int)]
runRegret g (TopRegret r) =
  runST $ do
    regretMap <- Mutable.Map.new
    accumMap <- Mutable.Map.new
    iteration <- newSTRef 0
    randSource <- MWC.initialize (MWC.fromSeed g)
    runReaderT r TopEnv{..}
    current <- readSTRef iteration
    map (\(k, x) -> (k, accumValue (update current x), visits x)) . sortOn fst <$>
      Mutable.Map.toList accumMap
