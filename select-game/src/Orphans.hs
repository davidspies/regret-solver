{-# OPTIONS_GHC -fno-warn-orphans #-}

module Orphans () where

import Data.Hashable (Hashable(..))
import qualified Data.Vector as DVec

instance Hashable a => Hashable (DVec.Vector a) where
  hashWithSalt salt = hashWithSalt salt . DVec.toList
