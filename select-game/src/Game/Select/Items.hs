{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Game.Select.Items where

import Data.DList (DList)
import Data.Hashable (Hashable(..))
import qualified Data.Vector as DVec
import GHC.Generics (Generic)

import Data.Some (ShowAll(..), Some(Some), UnParam(..))
import Orphans ()

class Items g where
  type Value g
  data Action g p
  data Phase g p
  data Reset g
  data Reveal g

data InfoSet' phase history action p = InfoSet
  { phase   :: phase p
  , history :: history
  , options :: DVec.Vector (action p)
  }
instance UnParam (InfoSet' phase history action) where
  data RemoveParam (InfoSet' phase history action) = UInfoSet
    { uphase   :: Some phase
    , uhistory :: history
    , uoptions :: DVec.Vector (Some action)
    }
    deriving (Generic)
  unparam InfoSet{phase, history, options} = UInfoSet
    { uphase   = Some phase
    , uhistory = history
    , uoptions = DVec.map Some options
    }

newtype ShowAllToShow f x = ShowAllToShow (f x)

instance ShowAll f => Show (ShowAllToShow f x) where
  showsPrec d (ShowAllToShow x) = showsPrecAll d x

deriving instance
  ( UnParam phase
  , UnParam action
  , Eq (RemoveParam action)
  , Eq (RemoveParam phase)
  , Eq history
  ) => Eq (RemoveParam (InfoSet' phase history action))
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
  showsPrecAll d InfoSet{phase, history, options} = showsPrec d InfoSet
    { phase = ShowAllToShow phase
    , history
    , options = DVec.map ShowAllToShow options
    }

instance
  ( UnParam action
  , UnParam phase
  , Hashable (RemoveParam action)
  , Hashable (RemoveParam phase)
  , Hashable history
  ) => Hashable (RemoveParam (InfoSet' phase history action))

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
