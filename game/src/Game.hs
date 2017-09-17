{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Game
    ( Items(..)
    , Game(..)
    ) where

import Data.Dist (Dist)
import qualified Data.Map.Generic as Map
import qualified Data.Map.Mutable.Generic as Mutable.Map
import qualified Data.Map.Mutable.Generic as Mutable (Map)
import Data.Vector.Class (Vector)
import Game.PlayerMap (PlayerIndex, PlayerMap)

class Items g where
  data State g
  data Action g
  data InfoSet g
  type Value g
  type ActionMap g :: * -> *
  type InfoMap g :: * -> * -> *

class ( Vector (Value g)
      , Mutable.Map (InfoMap g), Mutable.Map.Key (InfoMap g) ~ InfoSet g, Ord (InfoSet g)
      , Map.KeyTraversable (ActionMap g), Map.Key (ActionMap g) ~ Action g
      , Map.MapValue (ActionMap g) ()
      , Map.MapValue (ActionMap g) Float
      , Map.MapValue (ActionMap g) (Int, Float)
      , Map.MapValue (ActionMap g) (Value g)
      , Items g
      ) => Game g where
  getNumPlayers :: g -> Int
  getPrimitiveValue :: g -> State g -> Maybe (Value g)
  getActions :: g -> InfoSet g -> ActionMap g ()
  applyActions :: g -> PlayerMap (Action g) -> State g -> Dist (State g)
  getInfoSet :: g -> PlayerIndex -> State g -> Maybe (InfoSet g)
  getUtility :: g -> PlayerIndex -> Value g -> Float
  startState :: g -> State g
