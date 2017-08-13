{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}

module Game.Regret.Monad
    ( Regret
    , TopRegret
    , averageValue
    , addRegret
    , regretValue
    , runRegret
    , saveRegrets_
    ) where

import Control.Arrow (first, second)
import Control.Monad (ap, forM_, void, when)
import Control.Monad.Random (MonadRandom(..), StdGen, random, randomR, randomRs, randoms)
import qualified Control.Monad.Random as MonadRandom
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT, withReaderT)
import Control.Monad.ST (ST, runST)
import Data.Maybe (isNothing)
import Data.STRef (STRef, modifySTRef, newSTRef, readSTRef, writeSTRef)

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

data TopEnv m s v = TopEnv
  { regretMap  :: m s v
  , accumMap   :: m s v
  , randSource :: STRef s StdGen
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

randOp :: (StdGen -> (a, StdGen)) -> Regret m v a
randOp op = Regret $ do
  Env{tenv=TopEnv{randSource}} <- ask
  liftST $ do
    g <- readSTRef randSource
    let (res, nextG) = op g
    writeSTRef randSource nextG
    return res

instance MonadRandom (Regret m v) where
  getRandom = randOp random
  getRandoms = randOp (first randoms . MonadRandom.split)
  getRandomR = randOp . randomR
  getRandomRs r = randOp (first (randomRs r) . MonadRandom.split)

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
  TopEnv{regretMap, accumMap} <- ask
  liftSTTop $ do
    usedV <- readSTRef used
    forM_ usedV $ \k -> do
      Just val <- Mutable.Map.lookup k regretMap
      Mutable.Map.insertWith add k (untypedNormalize val) accumMap

regretValue :: (Normalizing v, Mutable.Map m) => Key m -> Regret m v (Maybe (Normal v))
regretValue k = Regret $ do
  Env{tenv = TopEnv{regretMap}} <- ask
  liftST $ fmap normalize <$> Mutable.Map.lookup k regretMap

averageValue :: (Normalizing v, Mutable.Map m) => Key m -> Regret m v (Maybe (Normal v))
averageValue k = Regret $ do
  Env{tenv = TopEnv{accumMap}} <- ask
  liftST $ fmap normalize <$> Mutable.Map.lookup k accumMap

runRegret :: (Mutable.Map m, Normalizing v)
  => StdGen -> TopRegret m v () -> [(Key m, Normal v)]
runRegret g (TopRegret r) =
  runST $ do
    regretMap <- Mutable.Map.new
    accumMap <- Mutable.Map.new
    randSource <- newSTRef g
    runReaderT r
      TopEnv
        { regretMap
        , accumMap
        , randSource
        }
    map (second normalize) <$> Mutable.Map.toList accumMap
