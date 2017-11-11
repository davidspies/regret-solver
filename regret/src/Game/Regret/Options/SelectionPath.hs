{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Game.Regret.Options.SelectionPath
    ( SelectionPath (..)
    , noPaths
    , nullPaths
    ) where

import Control.Arrow ((***))
import qualified Data.List.NonEmpty as NonEmpty

import Control.Monad.Random (MonadSTRandom)
import qualified Control.Monad.Random as Random
import Data.Dist (expected, sample)
import qualified Data.Dist as Dist

import Control.Monad.Scale (MonadScale, scaleBy)
import Game.Regret.Options.Class

newtype SelectionPath = SelectionPath Int

noPaths :: SelectionPath
noPaths = SelectionPath 0

nullPaths :: SelectionPath -> Bool
nullPaths (SelectionPath x) = x == 0

class (MonadSTRandom m, MonadScale m) => SelectionPathConstraints m
instance (MonadSTRandom m, MonadScale m) => SelectionPathConstraints m

secondM :: Applicative f => (a -> f b) -> (x, a) -> f (x, b)
secondM f (x, y) = (x,) <$> f y

instance RegretOptions SelectionPath where
  type MConstraints SelectionPath = SelectionPathConstraints
  selectChance path x continuation = sample x >>= continuation path
  selectOpposite path x continuation = sample x >>= continuation path
  selectSelf (SelectionPath npaths) xs ifVisited ifUnvisited
    | npaths > lenxs = (map ((1,) . snd) $ NonEmpty.toList pxs,) . expected . Dist.normalize <$>
        mapM (secondM $ ifVisited (SelectionPath (npaths `quot` lenxs))) pxs
    | npaths <= 0 = error "npaths: expected positive"
    | npaths == 1 = fmap ((: []) . (1 / fromIntegral lenxs,) . snd *** expected . Dist.normalize) $
        Random.uniformList pxs $ \selected -> secondM $ \x -> if selected
          then scaleBy (fromIntegral lenxs) $ ifVisited (SelectionPath 1) x
          else ifUnvisited x
    | otherwise = fmap (map ((fromIntegral npaths / fromIntegral lenxs,) . snd) *** expected . Dist.normalize . NonEmpty.fromList) $
        Random.uniformListSubset npaths (NonEmpty.toList pxs) $
          \selected -> secondM $ \x -> if selected
            then scaleBy (fromIntegral lenxs / fromIntegral npaths) $ ifVisited (SelectionPath 1) x
            else ifUnvisited x
    where
      pxs = Dist.pieces xs
      lenxs = length pxs
