{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Game.Select
    ( SelectGame (..)
    , Items (..)
    , Game (..)
    , SGM
    , module X
    ) where

import qualified Data.Strict.Maybe as Strict
import qualified Data.Vector as DVec

import qualified Data.Map.Generic as Map
import Data.Map.VecMap (VecMap(VecMap))
import qualified Game
import Game.PlayerMap (initPlayerMap)

import Control.Monad.Select.Internal (Packed(..), Packed'(..), Some(..), runSelect)
import qualified Control.Monad.Select.Internal as Select
import Game.Select.Helpers as X
import Game.Select.Internal
import Game.Select.Items

instance Game g => Game.Game (SelectGame g) where
  getNumPlayers (SelectGame g) = getNumPlayers g
  getPrimitiveValue _ (SGWState (Finished v)) = Just v
  getPrimitiveValue _ (SGWState (Option _ _)) = Nothing
  getActions _ (IS (Some InfoSet{options})) =
    AVMap $ VecMap $ DVec.replicate (DVec.length options) (Strict.Just ())
  applyActions _ _ (SGWState (Finished _)) = error "Game over"
  applyActions _ selection (SGWState (Option _ continue)) =
      asGS <$> continue (ActionInputs $ fmap aInd selection)
    where
      asGS (Packed ps) = SGWState ps
  getInfoSet _ _ (SGWState (Finished _))         = error "Game over"
  getInfoSet _ p (SGWState (Option (Infos m) _))
      | null (options i) = Nothing
      | otherwise = Just $ IS $ Some i
    where
      i = m Map.! p
  getUtility (SelectGame g) = getUtility g
  startState (SelectGame g) = case startInfo g of
    PlayerStart sp ->
      asSome $ Select.pack $ runSelect
        (game g)
        (Infos
          (initPlayerMap (getNumPlayers g)
          (Just . sp))
        )
    where
      asSome (Packed v) = SGWState v
