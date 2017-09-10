{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

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

import Data.Coerce (coerce)
import Data.Hashable (Hashable)
import Data.MemoTrie (HasTrie(..))
import qualified Data.Strict.Maybe as Strict
import qualified Data.Vector as DVec
import Prelude hiding (elem, lookup)

import Data.Map.Generic
import Data.Vector.Class (Vector)

newtype PlayerIndex = PI Int
  deriving (Eq, Ord, Show, Hashable)

-- Avoid unused constructor warning
_playerIndexTrie :: (Int :->: a) -> (PlayerIndex :->: a)
_playerIndexTrie = PlayerIndexTrie

instance HasTrie PlayerIndex where
  newtype (:->:) PlayerIndex a = PlayerIndexTrie (Int :->: a)
  trie = coerce (trie @Int)
  untrie = coerce (untrie @Int)
  enumerate = coerce (filter ((>= 0) . fst) . enumerate @Int)

newtype PlayerMap a = PlayerMap (VecMap a)
  deriving (Show, Functor, Foldable, Traversable, Vector, Map)

type instance Key PlayerMap = PlayerIndex

playerList :: Int -> [PlayerIndex]
playerList numPlayers = map PI [0..(numPlayers-1)]

initPlayerMap :: Int -> (PlayerIndex -> Maybe a) -> PlayerMap a
initPlayerMap numPlayers playerOp =
  PlayerMap $ VecMap $ DVec.generate numPlayers (maybe Strict.Nothing Strict.Just . playerOp . PI)
