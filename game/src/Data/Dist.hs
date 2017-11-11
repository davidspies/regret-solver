{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Data.Dist
    ( Dist
    , Probability
    , expected
    , normalize
    , pieces
    , sample
    , withProbability
    ) where

import Control.Arrow (first)
import Control.Monad (ap)
import Control.Monad.Random (Randomizable)
import Data.Dist.Internal (SDist)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

import qualified Data.Dist.Internal as SDist
import Data.Vector.Class

type Probability = Float

data Dist a = Bottom a | Level (SDist (Dist a))
  deriving (Functor, Show)

instance Applicative Dist where
  pure = return
  (<*>) = ap

instance Monad Dist where
  return = Bottom
  d >>= f =
    case d of
      Bottom b -> f b
      Level l  -> case SDist.extractSingleton l of
        Just x  -> x >>= f
        Nothing -> Level $ (>>= f) <$> l

expected :: Vector a => Dist a -> a
expected d = case d of
  Bottom s -> s
  Level s  -> SDist.expected $ expected <$> s

sample :: Randomizable m Float => Dist a -> m a
sample d = case d of
  Bottom s -> return s
  Level s  -> SDist.sample s >>= sample

fromSDist :: SDist a -> Dist a
fromSDist = Level . fmap Bottom

normalize :: NonEmpty (Float, a) -> Dist a
normalize = fromSDist . SDist.normalize

withProbability :: Dist a -> Dist (Probability, a)
withProbability (Bottom d) = Bottom (1, d)
withProbability (Level d) =
  Level $ (\(m, x) -> first (m *) <$> withProbability x) <$> SDist.withProbability d

pieces :: Dist a -> [(Probability, a)]
pieces d = go 1 d []
  where
    go mult = \case
      Bottom d' -> ((mult, d') :)
      Level d'  -> foldr1 (.) $ NonEmpty.map (uncurry $ go . (* mult)) $ SDist.nonZeroPieces d'
