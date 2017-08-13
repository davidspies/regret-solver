{-# LANGUAGE NamedFieldPuns #-}

module Game.Select.Helpers
    ( allSelect
    , noop
    , offTurnSelect
    , reset
    , reveal
    , revealAll
    , turnSelect
    ) where

import Control.Monad (void)
import qualified Data.Vector as DVec

import qualified Data.Map.Generic as Map
import Game.PlayerMap (PlayerIndex, PlayerMap)

import Control.Monad.Select.Internal (Some(Some), select)
import qualified Control.Monad.Select.Internal as Select
import Game.Select.Internal (ActionInputs(..), SGM, StateInfos(..))
import Game.Select.Items

doReveal :: Reveal g -> InfoSet g p -> InfoSet g p
doReveal r v@InfoSet{history=h@History{reveals}} = v{history=h{reveals = r : reveals}}

reveal :: PlayerIndex -> Reveal g -> SGM g ()
reveal i r =
  Select.updateState (\(Infos m) -> Some $ Infos $ Map.adjust (doReveal r) i m)

revealAll :: Reveal g -> SGM g ()
revealAll r =
  Select.updateState (\(Infos m) -> Some $ Infos $ fmap (doReveal r) m)

reset :: Reset g -> SGM g ()
reset r =
  Select.updateState
    (\(Infos m) ->
      Some $ Infos $ fmap (\v -> v{history = History{begin=r, reveals=[]}}) m
    )

turnSelect ::
  Phase g p -> PlayerIndex -> DVec.Vector (Action g p) -> SGM g (Action g p)
turnSelect p i acts =
  (\(ActionInputs m) -> acts DVec.! (m Map.! i))
  <$>
  Select.modify
    (\(Infos m) -> Infos $
      Map.mapWithKey
      (\j v -> v{phase=p, options=if i == j then acts else DVec.empty}) m
    )

offTurnSelect ::
  Phase g p -> PlayerIndex -> (PlayerIndex -> DVec.Vector (Action g p))
  -> SGM g (PlayerMap (Action g p))
offTurnSelect p i afn =
  (\(ActionInputs m) -> Map.mapWithKey (\j s -> afn j DVec.! s) m)
  <$>
  Select.modify
    (\(Infos m) -> Infos $
      Map.mapWithKey (\j v ->
        v{phase=p, options=if i == j then DVec.empty else afn j}
      ) m
    )

allSelect ::
     Phase g p -> (PlayerIndex -> DVec.Vector (Action g p))
  -> SGM g (PlayerMap (Action g p))
allSelect p afn =
  (\(ActionInputs m) -> Map.mapWithKey (\j s -> afn j DVec.! s) m)
  <$>
  Select.modify
    (\(Infos m) -> Infos $ Map.mapWithKey (\j v -> v{phase=p, options=afn j}) m)

noop :: SGM g ()
noop = do
  Select.updateState (\(Infos m) -> Some $ Infos $ fmap (\v -> v{options=DVec.empty}) m)
  void select
