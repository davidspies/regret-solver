{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Game.KuhnPoker (KuhnPoker (..)) where

import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Some (Some(Some), UnParam(..))
import qualified Data.Vector as DVec
import GHC.Generics (Generic)

import Control.Monad.Select (chance)
import qualified Data.Dist as Dist
import qualified Data.Vector.Class as Vector
import Game.PlayerMap (PlayerIndex, playerList)
import Game.Select
import qualified Game.Select as Select (Game)

checkOrBet :: DVec.Vector (Action KuhnPoker Betting)
checkOrBet = DVec.fromList [Check, Bet]
callOrFold :: DVec.Vector (Action KuhnPoker Calling)
callOrFold = DVec.fromList [Call, Fold]

instance Select.Game KuhnPoker where
  getNumPlayers KuhnPoker = 2
  getUtility KuhnPoker p d
    | p == leftPlayer = d
    | p == rightPlayer = -d
    | otherwise = error "Only a 2-player game."
  startReset KuhnPoker = Start
  startPhase KuhnPoker = Some Betting
  game KuhnPoker = do
    noop
    leftCard <- chance (Dist.normalize $ NonEmpty.map (1,) cards)
    reveal leftPlayer (Draw leftCard)
    rightCard <- chance (Dist.normalize $ NonEmpty.map (1,) $ cardsExcepting leftCard)
    reveal rightPlayer (Draw rightCard)
    leftAction <- turnSelect Betting leftPlayer checkOrBet
    revealAll (Acts leftPlayer $ Some leftAction)
    let compareVal = compareCards leftCard rightCard
    case leftAction of
      Bet -> do
        rightAction <- turnSelect Calling rightPlayer callOrFold
        case rightAction of
          Call -> return $ Vector.scale 2 compareVal
          Fold -> return 1
      Check -> do
        rightAction <- turnSelect Betting rightPlayer checkOrBet
        case rightAction of
          Check -> return compareVal
          Bet   -> do
            revealAll (Acts rightPlayer $ Some rightAction)
            secondLeftAction <- turnSelect Calling leftPlayer callOrFold
            return $
              case secondLeftAction of
                Fold -> -1
                Call -> Vector.scale 2 compareVal

data Card = Jack | Queen | King
  deriving (Eq, Ord, Show, Generic, Enum, Hashable)

cards :: NonEmpty Card
cards = NonEmpty.fromList [Jack ..]

cardsExcepting :: Card -> NonEmpty Card
cardsExcepting card = NonEmpty.fromList [c | c <- [Jack ..], c /= card]

data KuhnPoker = KuhnPoker

compareCards :: Card -> Card -> Value KuhnPoker
compareCards leftCard rightCard =
  case compare leftCard rightCard of
    LT -> -1
    EQ -> 0
    GT -> 1

data Betting
data Calling

instance Game.Select.Items KuhnPoker where
  data Reset KuhnPoker = Start
    deriving (Eq, Ord, Show, Generic, Hashable)
  data Reveal KuhnPoker =
      Draw Card
    | Acts PlayerIndex (Some (Action KuhnPoker))
    deriving (Eq, Ord, Show, Generic, Hashable)
  data Phase KuhnPoker p where
    Betting :: Phase KuhnPoker Betting
    Calling :: Phase KuhnPoker Calling
  data (Action KuhnPoker p) where
    Bet :: Action KuhnPoker Betting
    Check :: Action KuhnPoker Betting
    Call :: Action KuhnPoker Calling
    Fold :: Action KuhnPoker Calling
  type Value KuhnPoker = Double

instance UnParam (Action KuhnPoker) where
  data RemoveParam (Action KuhnPoker) = UBet | UCheck | UCall | UFold
    deriving (Eq, Ord, Generic, Hashable, Show)
  unparam = \case
    Bet   -> UBet
    Check -> UCheck
    Call  -> UCall
    Fold  -> UFold

instance UnParam (Phase KuhnPoker) where
  data RemoveParam (Phase KuhnPoker) = UBetting | UCalling
    deriving (Eq, Ord, Generic, Hashable, Show)
  unparam = \case
    Betting -> UBetting
    Calling -> UCalling

leftPlayer :: PlayerIndex
rightPlayer :: PlayerIndex
[leftPlayer, rightPlayer] = playerList 2
