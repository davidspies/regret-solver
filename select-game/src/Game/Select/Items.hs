{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Game.Select.Items where

import Data.Hashable (Hashable(..))
import qualified Data.Vector as DVec
import GHC.Generics (Generic)

import Data.Some (Some(Some), UnParam(..))
import Orphans ()

class Items g where
  type Value g
  data Action g p
  data Phase g p
  data Reset g
  data Reveal g

data InfoSet g p = InfoSet
  { phase   :: Phase g p
  , history :: History g
  , options :: DVec.Vector (Action g p)
  }
instance UnParam (InfoSet g) where
  data RemoveParam (InfoSet g) = UInfoSet
    { uphase   :: Some (Phase g)
    , uhistory :: History g
    , uoptions :: DVec.Vector (Some (Action g))
    }
    deriving (Generic)
  unparam InfoSet{phase, history, options} = UInfoSet
    { uphase   = Some phase
    , uhistory = history
    , uoptions = DVec.map Some options
    }

deriving instance
  ( UnParam (Phase g)
  , UnParam (Action g)
  , Eq (RemoveParam (Action g))
  , Eq (RemoveParam (Phase g))
  , Eq (Reset g)
  , Eq (Reveal g)
  ) => Eq (RemoveParam (InfoSet g))
deriving instance
  ( UnParam (Phase g)
  , UnParam (Action g)
  , Show (RemoveParam (Action g))
  , Show (RemoveParam (Phase g))
  , Show (Reset g)
  , Show (Reveal g)
  ) => Show (RemoveParam (InfoSet g))
instance
  ( UnParam (Action g)
  , UnParam (Phase g)
  , Hashable (RemoveParam (Action g))
  , Hashable (RemoveParam (Phase g))
  , Hashable (Reset g)
  , Hashable (Reveal g)
  ) => Hashable (RemoveParam (InfoSet g))

data History g = History
  { begin   :: Reset g
  , reveals :: [Reveal g]
  }
  deriving (Generic)
deriving instance (Eq (Reset g), Eq (Reveal g)) => Eq (History g)
deriving instance (Show (Reveal g), Show (Reset g)) => Show (History g)
instance (Hashable (Reset g), Hashable (Reveal g)) => Hashable (History g)
