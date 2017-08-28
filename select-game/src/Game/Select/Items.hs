{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Game.Select.Items
    ( History
    , History' (..)
    , InfoSet
    , InfoSet' (..)
    , Items (..)
    ) where

import Data.DList.Strict (DList)
import Data.Hashable (Hashable(..))
import qualified Data.Vector as DVec
import GHC.Generics (Generic)

import Game.PlayerMap (PlayerIndex)

import Data.Some (EqAll, HashableAll, OrdAll, ShowAll(..), Some(Some), UnParam(..))
import Orphans ()

class Items g where
  type Value g
  data Action g p
  data Phase g p
  data Reset g
  data Reveal g

data InfoSet' phase history action p = InfoSet
  { player  :: !PlayerIndex
  , phase   :: !(phase p)
  , history :: !history
  , options :: !(DVec.Vector (action p))
  }
instance UnParam (InfoSet' phase history action) where
  data RemoveParam (InfoSet' phase history action) = UInfoSet
    { uplayer  :: !PlayerIndex
    , uphase   :: !(Some phase)
    , uhistory :: !history
    , uoptions :: !(DVec.Vector (Some action))
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

determiningTuple
  :: RemoveParam (InfoSet' phase history action) -> (PlayerIndex, Some phase, history)
determiningTuple UInfoSet {uplayer, uphase, uhistory} = (uplayer, uphase, uhistory)

instance
  ( EqAll phase
  , Eq history
  ) => Eq (RemoveParam (InfoSet' phase history action)) where
  -- Ignore options
  (==) ui1 ui2 = determiningTuple ui1 == determiningTuple ui2
instance
  ( OrdAll phase
  , Ord history
  ) => Ord (RemoveParam (InfoSet' phase history action)) where
  -- Ignore options
  compare ui1 ui2 = compare (determiningTuple ui1) (determiningTuple ui2)

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
  ( HashableAll phase
  , Hashable history
  ) => Hashable (RemoveParam (InfoSet' phase history action)) where
  -- Ignoring options
  hashWithSalt d = hashWithSalt d . determiningTuple

data History' reset reveal = History
  { begin   :: !reset
  , reveals :: !(DList reveal)
  }
  deriving (Generic)

deriving instance (Eq reset, Eq reveal) => Eq (History' reset reveal)
deriving instance (Ord reset, Ord reveal) => Ord (History' reset reveal)
deriving instance (Show reveal, Show reset) => Show (History' reset reveal)
instance (Hashable reset, Hashable reveal) => Hashable (History' reset reveal)

instance (EqAll phase, Eq history) => EqAll (InfoSet' phase history action)
instance (OrdAll phase, Ord history) => OrdAll (InfoSet' phase history action)
instance (HashableAll phase, Hashable history) => HashableAll (InfoSet' phase history action)

type History g = History' (Reset g) (Reveal g)
type InfoSet g = InfoSet' (Phase g) (History g) (Action g)
