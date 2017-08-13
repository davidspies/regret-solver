{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Game.Dudo (Dudo (..)) where

import Data.Hashable (Hashable)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import qualified Data.Vector as DVec
import GHC.Generics (Generic)

import Control.Monad.Select (chance)
import qualified Data.Dist as Dist
import qualified Data.Map.Generic as Map
import Data.Some (Some(Some), UnParam(..))
import Game.PlayerMap (PlayerIndex, PlayerMap, initPlayerMap, playerList)
import Game.Select
import qualified Game.Select as Select (Game)

acceptOrChallenge, challengeOnly :: DVec.Vector (Action Dudo Challenging)
acceptOrChallenge = DVec.fromList [Accept, Challenge]
challengeOnly = DVec.singleton Challenge

instance Select.Game Dudo where
  getNumPlayers = numPlayers
  getUtility Dudo{} p = fromMaybe 0 . Map.lookup p
  startReset Dudo{numPlayers} = R {alive = playerList numPlayers, lastClaim = 0}
  startPhase Dudo{} = Some Claiming
  game Dudo{numPlayers, dieSides} = do
    let players = playerList numPlayers
        turns = cycle players
        listSides = NonEmpty.fromList [1 .. dieSides]
        claimOpts = DVec.fromList [Claim c | c <- [1 .. dieSides]]
    noop
    Just winner <-
      foldUntilRightM
        (\(alive, prevClaim) p ->
          if alive Map.! p then do
            roll <- chance $ Dist.normalize $ NonEmpty.map (1,) listSides
            reveal p (Roll roll)
            Claim playerClaim <-
              turnSelect Claiming p $
              DVec.slice prevClaim (dieSides - prevClaim) claimOpts
            revealAll (ClaimMade p playerClaim)
            challengeActs <- offTurnSelect Challenging p $ const $
              if playerClaim == dieSides
                then challengeOnly
                else acceptOrChallenge
            let
              challengers =
                map fst $
                filter ((\case {Challenge -> True; _ -> False}) . snd) $
                Map.toList challengeActs
              (nowAlive, nowClaim) =
                  if
                    | null challengers -> (alive, playerClaim)
                    | roll >= playerClaim ->
                        ( foldl
                            (flip $ Map.adjust $ const False)
                            alive
                            challengers
                        , playerClaim)
                    | otherwise -> (Map.adjust (const False) p alive, prevClaim)
              aliveList = map fst $ filter snd $ Map.toList nowAlive
            if null $ tail aliveList then
                return $ Right $ head aliveList
              else do
                reset R{alive = aliveList, lastClaim = nowClaim}
                return (Left (nowAlive, nowClaim))
          else return (Left (alive, prevClaim))
      ) (initPlayerMap numPlayers (const $ Just True), 0) turns
    return $ pwin numPlayers winner

data Dudo = Dudo
  { numPlayers :: Int
  , dieSides   :: Int
  }

pwin :: Int -> PlayerIndex -> Value Dudo
pwin numPlayers p =
  Map.adjust (+ fromIntegral numPlayers) p
    (initPlayerMap numPlayers $ const $ Just (-1))

foldUntilRightM :: Monad m
  => (b -> a -> m (Either b c)) -> b -> [a] -> m (Maybe c)
foldUntilRightM op = go
  where
    go _ [] = return Nothing
    go base (item : items) = do
      r <- op base item
      case r of
        Left v  -> go v items
        Right w -> return (Just w)

data Claiming
data Challenging

instance Game.Select.Items Dudo where
  type Value Dudo = PlayerMap Double
  data Phase Dudo p where
    Challenging :: Phase Dudo Challenging
    Claiming :: Phase Dudo Claiming
  data Action Dudo p where
    Claim :: Int -> Action Dudo Claiming
    Accept :: Action Dudo Challenging
    Challenge :: Action Dudo Challenging
  data Reveal Dudo = Roll Int | ClaimMade PlayerIndex Int
    deriving (Eq, Ord, Show, Generic, Hashable)
  data Reset Dudo = R {alive :: [PlayerIndex], lastClaim :: Int}
    deriving (Eq, Ord, Show, Generic, Hashable)

instance UnParam (Action Dudo) where
  data RemoveParam (Action Dudo) = UClaim Int | UAccept | UChallenge
    deriving (Eq, Generic, Hashable, Show)
  unparam = \case
    Claim n   -> UClaim n
    Accept    -> UAccept
    Challenge -> UChallenge

instance UnParam (Phase Dudo) where
  data RemoveParam (Phase Dudo) = UChallenging | UClaiming
    deriving (Eq, Generic, Hashable, Show)
  unparam = \case
    Challenging -> UChallenging
    Claiming    -> UClaiming
