{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Game.PlayerMap
    ( PlayerMap
    , PlayerIndex
    , lookup
    , empty
    , adjust
    , size
    , mapWithKey
    , toList
    , elems
    , fromList
    , (!)
    , elem
    , playerList
    , initPlayerMap
    ) where

import Data.Hashable (Hashable)
import qualified Data.Vector as DVec
import Prelude hiding (elem, lookup)

import Data.Map.Generic
import Data.Vector.Class (Vector)

newtype PlayerIndex = PI Int
  deriving (Eq, Ord, Show, Hashable)

newtype PlayerMap a = PlayerMap (VecMap a)
  deriving (Show, Functor, Foldable, Traversable, Vector, Map)

type instance Key PlayerMap = PlayerIndex

playerList :: Int -> [PlayerIndex]
playerList numPlayers = map PI [0..(numPlayers-1)]

initPlayerMap :: Int -> (PlayerIndex -> Maybe a) -> PlayerMap a
initPlayerMap numPlayers playerOp =
  PlayerMap $ VecMap $ DVec.generate numPlayers (playerOp . PI)
