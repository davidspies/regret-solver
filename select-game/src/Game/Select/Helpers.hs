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
import qualified Data.DList as DList
import qualified Data.Vector as DVec

import qualified Data.Map.Generic as Map
import Game.PlayerMap (PlayerIndex, PlayerMap, initPlayerMap)

import Control.Monad.Select.Internal (Some(Some), select)
import qualified Control.Monad.Select.Internal as Select
import Game.Select.Internal (ActionInputs(..), SGM, StateInfos(..))
import Game.Select.Items

doReveal :: Reveal g -> InfoSet g p -> InfoSet g p
doReveal r v@InfoSet{history=h@History{reveals}} = v{history=h{reveals = reveals `DList.snoc` r}}

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
      Some $ Infos $ fmap (\v -> v{history = History{begin=r, reveals=DList.empty}}) m
    )

data PlayerOptions g p = PlayerOptions (Phase g p) (PlayerMap (DVec.Vector (Action g p)))

turnOptions :: Phase g p -> PlayerIndex -> DVec.Vector (Action g p) -> PlayerOptions g p
turnOptions p i acts =
  PlayerOptions p (initPlayerMap (\j -> Just $ if j == i then acts else DVec.empty))

turnSelect :: Phase g p -> PlayerIndex -> DVec.Vector (Action g p) -> SGM g (Action g p)
turnSelect p i acts = (Map.! i) <$> optionsSelect (turnOptions p i acts)

offTurnOptions :: Phase g p -> PlayerIndex -> (PlayerIndex -> DVec.Vector (Action g p))
  -> PlayerOptions g p
offTurnOptions p i afn =
  PlayerOptions p (initPlayerMap (\j -> Just $ if j == i then DVec.empty else afn j))

offTurnSelect :: Phase g p -> PlayerIndex -> (PlayerIndex -> DVec.Vector (Action g p))
  -> SGM g (PlayerMap (Action g p))
offTurnSelect p i afn = optionsSelect (offTurnOptions p i afn)

allOptions :: Phase g p -> (PlayerIndex -> DVec.Vector (Action g p)) -> PlayerOptions g p
allOptions p afn = PlayerOptions p (initPlayerMap (Just . afn))

allSelect :: Phase g p -> (PlayerIndex -> DVec.Vector (Action g p))
  -> SGM g (PlayerMap (Action g p))
allSelect p afn = optionsSelect (allOptions p afn)

optionsSelect :: PlayerOptions g p -> SGM g (PlayerMap (Action g p))
optionsSelect (PlayerOptions phase playerOptions) =
  Map.intersectionWith (DVec.!) playerOptions . (\(ActionInputs m) -> m)
  <$>
  Select.modify (\(Infos infos) ->
    Infos $ Map.intersectionWith (\state options -> state{phase, options}) infos playerOptions
  )

noop :: SGM g ()
noop = do
  Select.updateState (\(Infos m) -> Some $ Infos $ fmap (\v -> v{options=DVec.empty}) m)
  void select
