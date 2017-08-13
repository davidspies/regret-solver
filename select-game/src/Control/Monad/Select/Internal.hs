{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Control.Monad.Select.Internal
    ( Packed' (..)
    , Packed (..)
    , Select
    , Some (..)
    , chance
    , modify
    , pack
    , runSelect
    , select
    , updateState
    ) where

import Control.Monad (ap)

import Data.Dist (Dist)

import Data.Some (Some(..))

data Select' s a b where
  Finished' :: s t -> b -> Select' s a b
  Option' :: s t -> (a t -> Select' s a b) -> Select' s a b
  Chance' :: Dist (Select' s a b) -> Select' s a b

deriving instance Functor (Select' s a)

newtype Select s a b = Select {runSelect :: forall t. s t -> Select' s a b}

deriving instance Functor (Select s a)

instance Applicative (Select s a) where
  pure = return
  (<*>) = ap

instance Monad (Select s a) where
  return x = Select (`Finished'` x)
  (>>=) :: forall x y. Select s a x -> (x -> Select s a y) -> Select s a y
  Select infunc >>= f = Select outfunc
    where
      outfunc :: forall t. s t -> Select' s a y
      outfunc s = op (infunc s)
      op (Finished' s' z) = runSelect (f z) s'
      op (Option' s' zfn) = Option' s' (op . zfn)
      op (Chance' d)      = Chance' $ fmap op d

modify :: forall s t a. (forall t'. s t' -> s t) -> Select s a (a t)
modify func = Select (\s -> Option' (func s) (Finished' s))

select :: Select s a (Some a)
select = Select (\s -> Option' s (Finished' s . Some))

chance :: Dist b -> Select s a b
chance d = Select (\s -> Chance' (Finished' s <$> d))

updateState :: (forall t. s t -> Some s) -> Select s a ()
updateState alterfn = Select $ \s ->
  case alterfn s of
    Some res -> Finished' res ()

data Packed s a b = forall t. Packed (Packed' s a t b)

data Packed' s a t b where
  Finished :: b -> Packed' s a t b
  Option :: s t -> (a t -> Dist (Packed s a b)) -> Packed' s a t b

pack :: Select' s a b -> Packed s a b
pack (Finished' _ x) = Packed $ Finished x
pack (Chance' _) = error "Chance node cannot come before first option."
pack (Option' s choice) = Packed $ Option s (helper . choice)
  where
    helper (Finished' _ x)      = return $ Packed $ Finished x
    helper (Option' s' choice') = return $ Packed $ Option s' (helper . choice')
    helper (Chance' d)          = d >>= helper
