{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Game.Select.Internal where

import Data.Default (Default)
import qualified Data.DList as DList
import Data.Hashable (Hashable(..))
import qualified Data.Vector as DVec
import Data.Vector.Unboxed (Unbox)
import GHC.Generics (Generic)

import qualified Data.Map.Generic as Map
import qualified Data.Map.Mutable.Generic as MMap
import qualified Data.Map.VecMap.Unboxed as VUM
import Data.Vector.Class (Vector)
import qualified Game
import Game.PlayerMap (PlayerIndex, PlayerMap)

import Control.Monad.Select.Internal (Select, Some)
import qualified Control.Monad.Select.Internal as Select
import Data.Some
import Game.Select.Items

newtype SelectGame g = SelectGame g

newtype ActionInputs g p = ActionInputs (PlayerMap Int)

type SGM g = Select (StateInfos g) (ActionInputs g)

class
  ( OrdAll (Action g)
  , OrdAll (Phase g)
  , Ord (Reset g)
  , Ord (Reveal g)
  , HashableAll (Phase g)
  , Hashable (Reset g)
  , Hashable (Reveal g)
  , Items g
  , Default (Value g)
  , Unbox (Value g)
  , Vector (Value g)
  ) => Game g where
  getNumPlayers :: g -> Int
  getUtility :: g -> PlayerIndex -> Value g -> Float
  game :: g -> SGM g (Value g)
  startState :: g -> (Reset g, Some (Phase g))

data PlayerStart g = forall p. PlayerStart (PlayerIndex -> InfoSet g p)

{-# ANN startInfo "HLint: ignore Use const" #-}
startInfo :: Game g => g -> PlayerStart g
startInfo g = case startState g of
  (reset, Some phase) -> PlayerStart $ \player -> InfoSet
    { player
    , history = History {begin = reset, reveals = DList.empty}
    , phase
    , options = DVec.empty
    }

newtype StateInfos g p = Infos (PlayerMap (InfoSet g p))

type instance Map.MapValue (AVMap g) a = (Default a, Unbox a)
newtype AVMap g v = AVMap (VUM.VecMap v)
  deriving (Map.Map, Show)

type instance Map.Key (AVMap g) = Game.Action (SelectGame g)

instance Map.KeyTraversable (AVMap g) where
  traverseWithKey func (AVMap vm) = AVMap <$> Map.traverseWithKey (func . A) vm

instance Game.Items (SelectGame g) where
  data State (SelectGame g) = forall p.
    SGWState (Select.Packed' (StateInfos g) (ActionInputs g) p (Value g))
  type Value (SelectGame g) = Value g
  newtype InfoSet (SelectGame g) = IS (Some (InfoSet g))
    deriving (Generic)
  newtype Action (SelectGame g) = A {aInd :: Int}
    deriving (Eq, Ord, Show)
  type InfoMap (SelectGame g) = MMap.HTable (Game.InfoSet (SelectGame g))
  type ActionMap (SelectGame g) = AVMap g

deriving instance
  ( EqAll (Phase g)
  , Eq (Reset g)
  , Eq (Reveal g)
  ) => Eq (Game.InfoSet (SelectGame g))
deriving instance
  ( OrdAll (Phase g)
  , Ord (Reset g)
  , Ord (Reveal g)
  ) => Ord (Game.InfoSet (SelectGame g))
deriving instance
  ( HashableAll (Phase g)
  , Hashable (Reset g)
  , Hashable (Reveal g)
  ) => Hashable (Game.InfoSet (SelectGame g))
deriving instance
  ( ShowAll (Action g)
  , ShowAll (Phase g)
  , Show (Reset g)
  , Show (Reveal g)
  ) => Show (Game.InfoSet (SelectGame g))
