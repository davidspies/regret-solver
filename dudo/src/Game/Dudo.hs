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
import qualified Data.List.NonEmpty as NonEmpty
import Data.MemoTrie (memo, memo2)
import qualified Data.Vector as DVec
import GHC.Generics (Generic)

import Control.Monad.Select (chance)
import qualified Data.Dist as Dist
import Data.Some (EqAll, HashableAll, OrdAll, ShowAll(..), Some(Some), UnParam(..))
import Game.PlayerMap (PlayerIndex, PlayerMap)
import qualified Game.PlayerMap as PlayerMap
import qualified Game.PlayerMap as Player
import Game.Select
import qualified Game.Select as Select (Game)

acceptOrChallenge :: DVec.Vector (Action Dudo Challenging)
acceptOrChallenge = DVec.fromList [Accept, Challenge]

startReset :: Reset Dudo
startReset = R {lastClaim = 0}

instance Select.Game Dudo where
  getUtility Dudo{} = \case {Player.Left -> id; Player.Right -> negate}
  startState Dudo{} = (startReset, Some Claiming)
  game Dudo{dieSides} = do
    let
      claimOpts = DVec.fromList [Claim c | c <- [1 .. dieSides - 1]]
      claimSelect :: PlayerIndex -> Int -> SGM Dudo (Action Dudo Claiming)
      claimSelect = memo2 $ \r lastClaim -> turnSelect Claiming r (DVec.drop lastClaim claimOpts)
      challengeSelect :: [PlayerIndex] -> SGM Dudo (PlayerMap (Action Dudo Challenging))
      challengeSelect = memo $ \alive ->
        allSelect Challenging (\p -> if p `elem` alive then acceptOrChallenge else DVec.empty)
      dieChance :: SGM Dudo Int
      dieChance = chance $ Dist.normalize $ NonEmpty.map (1,) $ NonEmpty.fromList [1 .. dieSides]
      takeTurn
        :: (PlayerIndex, Reset Dudo) -> SGM Dudo (Either PlayerIndex (PlayerIndex, Reset Dudo))
      takeTurn (r, R{lastClaim}) = do
        dieRoll <- dieChance
        if
          | dieRoll == dieSides -> return $ Left r
          | lastClaim >= dieSides - 1 -> return $ Left $ Player.opposite r
          | otherwise -> do
            reveal r (if dieRoll > lastClaim then Roll dieRoll else UnderRoll)
            Claim claim <- claimSelect r lastClaim
            revealAll (ClaimMade r claim)
            challengeSelections <- challengeSelect [Player.opposite r]
            let
              cs = PlayerMap.toList challengeSelections
              challengers = [p | (p, a) <- cs, a == Challenge]
            if
              | null challengers -> do
                  reset R{lastClaim = claim}
                  return (Right (Player.opposite r, R{lastClaim = claim}))
              | dieRoll < claim -> return $ Left $ Player.opposite r
              | otherwise -> return $ Left r
    noop
    winner <- iterateUntilLeft takeTurn (Right (Player.Left, startReset))
    return $ pwin winner

iterateUntilLeft :: Monad m => (a -> m (Either b a)) -> Either b a -> m b
iterateUntilLeft op = either return go
  where
    go = (either return go =<<) . op

newtype Dudo = Dudo{dieSides :: Int}

pwin :: PlayerIndex -> Value Dudo
pwin Player.Left  = 1
pwin Player.Right = -1

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
  newtype Reset Dudo = R {lastClaim :: Int}
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
