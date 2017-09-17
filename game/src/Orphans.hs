{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Orphans () where

import qualified Data.Strict.Maybe as Strict

deriving instance Foldable Strict.Maybe
deriving instance Traversable Strict.Maybe
