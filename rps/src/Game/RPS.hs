{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Game.RPS(RPS (..)) where

import qualified Data.Map.Strict as StrictMap

import qualified Data.Map.Generic as Map
import qualified Data.Map.Mutable.Generic as MMap
import Game
import Game.PlayerMap (PlayerIndex, playerList)

data RPS = RPS

newtype IMRPS a = IMRPS (StrictMap.Map PlayerIndex a)
  deriving (Functor, Foldable, Map.Map, Show, Traversable)

type instance Map.Key IMRPS = InfoSet RPS

instance Game.Items RPS where
  data State RPS = PreMove | PostMove (Action RPS) (Action RPS)
    deriving (Show)
  type Value RPS = Float
  newtype (InfoSet RPS) = RPSIS PlayerIndex
    deriving (Eq, Ord, Show)
  data (Action RPS) = Rock | Paper | Scissors
    deriving (Eq, Ord, Show)
  type ActionMap RPS = StrictMap.Map (Action RPS)
  type InfoMap RPS = MMap.STMap IMRPS

instance Game RPS where
  getNumPlayers RPS = 2
  getPrimitiveValue RPS PreMove                      = Nothing
  getPrimitiveValue RPS (PostMove Rock Rock)         = Just 0
  getPrimitiveValue RPS (PostMove Rock Paper)        = Just (-1)
  getPrimitiveValue RPS (PostMove Rock Scissors)     = Just 10
  getPrimitiveValue RPS (PostMove Paper Rock)        = Just 1
  getPrimitiveValue RPS (PostMove Paper Paper)       = Just 0
  getPrimitiveValue RPS (PostMove Paper Scissors)    = Just (-1)
  getPrimitiveValue RPS (PostMove Scissors Rock)     = Just (-1)
  getPrimitiveValue RPS (PostMove Scissors Paper)    = Just 1
  getPrimitiveValue RPS (PostMove Scissors Scissors) = Just 0
  getActions RPS =
    const (StrictMap.fromList [(Rock, ()), (Paper, ()), (Scissors, ())])
  applyActions RPS acts =
    const $ return $ PostMove (acts Map.! leftPlayer) (acts Map.! rightPlayer)
  getInfoSet RPS = const . Just . RPSIS
  getUtility RPS p v
    | p == leftPlayer = v
    | p == rightPlayer = -v
    | otherwise = error "Only a 2-player game"
  startState RPS = PreMove

leftPlayer :: PlayerIndex
rightPlayer :: PlayerIndex
[leftPlayer, rightPlayer] = playerList 2
