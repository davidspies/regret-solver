{-# OPTIONS_GHC -fno-warn-orphans #-}

module Orphans () where

import Data.DList (DList)
import qualified Data.DList as DList
import Data.Hashable (Hashable(..))
import qualified Data.Vector as DVec

instance Hashable a => Hashable (DList a) where
  hashWithSalt salt = hashWithSalt salt . DList.toList

instance Hashable a => Hashable (DVec.Vector a) where
  hashWithSalt salt = hashWithSalt salt . DVec.toList
