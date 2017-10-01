{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Orphans () where

import Data.Default (Default, def)
import qualified Data.Strict.Maybe as Strict
import Data.Vector.Unboxed (Unbox)
import Data.Vector.Unboxed.Deriving (derivingUnbox)

deriving instance Foldable Strict.Maybe
deriving instance Traversable Strict.Maybe

instance Applicative Strict.Maybe where
  pure = Strict.Just
  (<*>) Strict.Nothing _                = Strict.Nothing
  (<*>) _ Strict.Nothing                = Strict.Nothing
  (<*>) (Strict.Just f) (Strict.Just x) = Strict.Just $ f x

derivingUnbox "Maybe"
    [t| forall a. (Default a, Unbox a) => Maybe a -> (Bool, a) |]
    [| maybe (False, def) (\ x -> (True, x)) |]
    [| \ (b, x) -> if b then Just x else Nothing |]
