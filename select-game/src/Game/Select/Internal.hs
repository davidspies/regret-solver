{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Game.Select.Internal where

import Data.Hashable (Hashable(..))
import qualified Data.Vector as DVec
import GHC.Generics (Generic)

import qualified Data.Map.Generic as Map
import qualified Data.Map.Mutable.Generic as MMap
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
  ( Eq (RemoveParam (Action g))
  , Eq (RemoveParam (Phase g))
  , Eq (Reset g)
  , Eq (Reveal g)
  , Hashable (RemoveParam (Action g))
  , Hashable (RemoveParam (Phase g))
  , Hashable (Reset g)
  , Hashable (Reveal g)
  , Items g
  , UnParam (Action g)
  , UnParam (Phase g)
  , Vector (Value g)
  ) => Game g where
  getNumPlayers :: g -> Int
  getUtility :: g -> PlayerIndex -> Value g -> Double
  game :: g -> SGM g (Value g)
  startReset :: g -> Reset g
  startPhase :: g -> Some (Phase g)

startInfo :: Game g => g -> Phase g p -> InfoSet g p
startInfo g p =
  InfoSet
    { history = History {begin = startReset g, reveals = []}
    , phase = p
    , options = DVec.empty
    }

newtype StateInfos g p = Infos (PlayerMap (InfoSet g p))

newtype AVMap g v = AVMap (Map.VecMap v)
  deriving (Functor, Foldable, Traversable, Map.Map, Show)

type instance Map.Key (AVMap g) = Game.Action (SelectGame g)

instance Items g => Game.Items (SelectGame g) where
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
  ( Eq (RemoveParam (Action g))
  , Eq (RemoveParam (Phase g))
  , Eq (Reset g)
  , Eq (Reveal g)
  , UnParam (Action g)
  , UnParam (Phase g)
  ) => Eq (Game.InfoSet (SelectGame g))
deriving instance Hashable (RemoveParam (InfoSet g)) => Hashable (Game.InfoSet (SelectGame g))
deriving instance Show (RemoveParam (InfoSet g)) => Show (Game.InfoSet (SelectGame g))