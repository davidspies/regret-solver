{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Game.Select.Items where

import Data.DList (DList)
import Data.Hashable (Hashable(..))
import qualified Data.Vector as DVec
import GHC.Generics (Generic)

import Game.PlayerMap (PlayerIndex)

import Data.Some (ShowAll(..), Some(Some), UnParam(..))
import Orphans ()

class Items g where
  type Value g
  data Action g p
  data Phase g p
  data Reset g
  data Reveal g

data InfoSet' phase history action p = InfoSet
  { player  :: PlayerIndex
  , phase   :: phase p
  , history :: history
  , options :: DVec.Vector (action p)
  }
instance UnParam (InfoSet' phase history action) where
  data RemoveParam (InfoSet' phase history action) = UInfoSet
    { uplayer  :: PlayerIndex
    , uphase   :: Some phase
    , uhistory :: history
    , uoptions :: DVec.Vector (Some action)
    }
    deriving (Generic)
  unparam InfoSet{..} = UInfoSet
    { uplayer  = player
    , uphase   = Some phase
    , uhistory = history
    , uoptions = DVec.map Some options
    }

newtype ShowAllToShow f x = ShowAllToShow (f x)

instance ShowAll f => Show (ShowAllToShow f x) where
  showsPrec d (ShowAllToShow x) = showsPrecAll d x

instance
  ( UnParam phase
  , Eq (RemoveParam phase)
  , Eq history
  ) => Eq (RemoveParam (InfoSet' phase history action)) where
  -- Ignore options
  (==) UInfoSet{uplayer=pl1, uphase=ph1, uhistory=h1}
       UInfoSet{uplayer=pl2, uphase=ph2, uhistory=h2}
    = (pl1, ph1, h1) == (pl2, ph2, h2)
deriving instance
  ( UnParam phase
  , UnParam action
  , Ord (RemoveParam action)
  , Ord (RemoveParam phase)
  , Ord history
  ) => Ord (RemoveParam (InfoSet' phase history action))
deriving instance
  ( Show (action p)
  , Show (phase p)
  , Show history
  ) => Show (InfoSet' phase history action p)
instance
  ( ShowAll action
  , ShowAll phase
  , Show history
  ) => ShowAll (InfoSet' phase history action) where
  showsPrecAll d InfoSet{..} = showsPrec d InfoSet
    { player
    , phase = ShowAllToShow phase
    , history
    , options = DVec.map ShowAllToShow options
    }

instance
  ( UnParam phase
  , Hashable (RemoveParam phase)
  , Hashable history
  ) => Hashable (RemoveParam (InfoSet' phase history action)) where
  -- Ignoring options
  hashWithSalt d UInfoSet{uplayer, uphase, uhistory} = hashWithSalt d (uplayer, uphase, uhistory)

data History' reset reveal = History
  { begin   :: reset
  , reveals :: DList reveal
  }
  deriving (Generic)

deriving instance (Eq reset, Eq reveal) => Eq (History' reset reveal)
deriving instance (Ord reset, Ord reveal) => Ord (History' reset reveal)
deriving instance (Show reveal, Show reset) => Show (History' reset reveal)
instance (Hashable reset, Hashable reveal) => Hashable (History' reset reveal)

type History g = History' (Reset g) (Reveal g)
type InfoSet g = InfoSet' (Phase g) (History g) (Action g)
