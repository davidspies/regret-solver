{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Game.Dudo (Dudo (..)) where

import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.MemoTrie (memo, memo2)
import qualified Data.Vector as DVec
import GHC.Generics (Generic)

import Control.Monad.Select (chance)
import qualified Data.Dist as Dist
import Data.Some (EqAll, HashableAll, OrdAll, ShowAll(..), Some(Some), UnParam(..))
import Game.PlayerMap (PlayerIndex, PlayerMap, playerList)
import qualified Game.PlayerMap as PlayerMap
import Game.Select
import qualified Game.Select as Select (Game)

acceptOrChallenge :: DVec.Vector (Action Dudo Challenging)
acceptOrChallenge = DVec.fromList [Accept, Challenge]

startReset :: Int -> Reset Dudo
startReset numPlayers = R {alive = NonEmpty.fromList $ playerList numPlayers, lastClaim = 0}

player1, player2 :: PlayerIndex
(player1, player2) = case playerList 2 of
  [p1, p2] -> (p1, p2)
  _        -> error "Unreachable"

instance Select.Game Dudo where
  getNumPlayers = numPlayers
  getUtility Dudo{} p
    | p == player1 = id
    | p == player2 = negate
    | otherwise = error "Only 2 players currently supported"
  startState Dudo{numPlayers} = (startReset numPlayers, Some Claiming)
  game g@Dudo{numPlayers, dieSides} = do
    let
      claimOpts = DVec.fromList [Claim c | c <- [1 .. dieSides - 1]]
      claimSelect :: PlayerIndex -> Int -> SGM Dudo (Action Dudo Claiming)
      claimSelect = memo2 $ \r lastClaim -> turnSelect g Claiming r (DVec.drop lastClaim claimOpts)
      challengeSelect :: [PlayerIndex] -> SGM Dudo (PlayerMap (Action Dudo Challenging))
      challengeSelect = memo $ \alive ->
        allSelect g Challenging (\p -> if p `elem` alive then acceptOrChallenge else DVec.empty)
      dieChance :: SGM Dudo Int
      dieChance = chance $ Dist.normalize $ NonEmpty.map (1,) $ NonEmpty.fromList [1 .. dieSides]
      takeTurn
        :: (PlayerIndex, Reset Dudo) -> SGM Dudo (Either PlayerIndex (PlayerIndex, Reset Dudo))
      takeTurn (r, R{alive, lastClaim}) = do
        dieRoll <- dieChance
        if
          | dieRoll == dieSides -> return $ Left r
          | lastClaim >= dieSides - 1 -> return $ extractPlayer R{alive, lastClaim}
          | otherwise -> do
            reveal r (if dieRoll > lastClaim then Roll dieRoll else UnderRoll)
            Claim claim <- claimSelect r lastClaim
            revealAll (ClaimMade r claim)
            challengeSelections <- challengeSelect $ NonEmpty.toList alive
            let
              cs = PlayerMap.toList challengeSelections
              challengers = [p | (p, a) <- cs, a == Challenge]
              accepters = [p | (p, a) <- cs, a == Accept]
              result = if
                | dieRoll < claim && not (null challengers) -> R {alive, lastClaim}
                | otherwise -> R{alive = neSnoc accepters r, lastClaim = claim}
            reset result
            return (extractPlayer result)
    noop
    winner <- iterateUntilLeft takeTurn (extractPlayer (startReset numPlayers))
    return $ pwin numPlayers winner

extractPlayer :: Reset Dudo -> Either PlayerIndex (PlayerIndex, Reset Dudo)
extractPlayer R{alive = p1 :| ps, lastClaim = claim} = case ps of
  []         -> Left p1
  (p2 : ps') -> Right (p1, R{alive = p2 :| ps', lastClaim = claim})

iterateUntilLeft :: Monad m => (a -> m (Either b a)) -> Either b a -> m b
iterateUntilLeft op = either return go
  where
    go = (either return go =<<) . op

neSnoc :: [a] -> a -> NonEmpty a
neSnoc = \case
  [] -> (:| [])
  (x : xs) -> (x :|) . (xs ++) . (: [])

data Dudo = Dudo
  { numPlayers :: Int
  , dieSides   :: Int
  }

pwin :: Int -> PlayerIndex -> Value Dudo
pwin 2 p
  | p == player1 = 1.0
  | p == player2 = -1.0
  | otherwise = error "player out of bounds"
pwin _ _ = error "Only 2 players currently supported"

data Claiming
data Challenging

instance Game.Select.Items Dudo where
  type Value Dudo = Float
  data Phase Dudo p where
    Challenging :: Phase Dudo Challenging
    Claiming :: Phase Dudo Claiming
    deriving (EqAll, OrdAll, HashableAll)
  data Action Dudo p where
    Claim :: Int -> Action Dudo Claiming
    Accept :: Action Dudo Challenging
    Challenge :: Action Dudo Challenging
    deriving (EqAll, OrdAll)
  data Reveal Dudo = UnderRoll | Roll Int | ClaimMade PlayerIndex Int
    deriving (Eq, Ord, Show, Generic, Hashable)
  data Reset Dudo = R {alive :: NonEmpty PlayerIndex, lastClaim :: Int}
    deriving (Eq, Ord, Show, Generic, Hashable)

deriving instance Eq (Action Dudo p)

deriving instance Show (Action Dudo p)
deriving instance Show (Phase Dudo p)

instance ShowAll (Action Dudo) where
  showsPrecAll = showsPrec
instance ShowAll (Phase Dudo) where
  showsPrecAll = showsPrec

instance UnParam (Action Dudo) where
  data RemoveParam (Action Dudo) = UClaim Int | UAccept | UChallenge
    deriving (Eq, Ord, Generic)
  unparam = \case
    Claim n   -> UClaim n
    Accept    -> UAccept
    Challenge -> UChallenge

instance UnParam (Phase Dudo) where
  data RemoveParam (Phase Dudo) = UChallenging | UClaiming
    deriving (Eq, Ord, Generic, Hashable)
  unparam = \case
    Challenging -> UChallenging
    Claiming    -> UClaiming
