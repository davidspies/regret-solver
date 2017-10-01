{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Game.PlayerMap
    ( PlayerMap
    , PlayerIndex(..)
    , lookup
    , empty
    , adjust
    , size
    , mapWithKey
    , toList
    , elems
    , fromList
    , (!)
    , elem
    , playerList
    , initPlayerMap
    , opposite
    ) where

import Data.Hashable (Hashable)
import Data.List (foldl')
import Data.MemoTrie (HasTrie(..))
import qualified Data.Strict.Maybe as Strict
import GHC.Generics (Generic)
import Orphans ()
import Prelude hiding (Either(..), elem, lookup)

import Data.Map.Generic
import qualified Data.Maybe.Util as Maybe
import qualified Data.Strict.Maybe.Util as Strict (catMaybes)
import qualified Data.Strict.Maybe.Util as Strict.Maybe
import Data.Vector.Class
    (Vector(..), genericAddMap, genericScaleMap, genericVNegateMap, genericVSumMap, genericZeroMap)

data PlayerIndex = Left | Right
  deriving (Eq, Ord, Show, Enum, Generic, Hashable)

instance HasTrie PlayerIndex where
  data (:->:) PlayerIndex a = PlayerTrie a a
  trie f = PlayerTrie (f Left) (f Right)
  untrie (PlayerTrie l r) = \case {Left -> l; Right -> r}
  enumerate (PlayerTrie l r) = [(Left, l), (Right, r)]

data PlayerMap a = PlayerMap !(Strict.Maybe a) !(Strict.Maybe a)
  deriving (Show, Functor, Foldable, Traversable)
type instance MapValue PlayerMap a = ()

type instance Key PlayerMap = PlayerIndex

instance Map PlayerMap where
  lookup k (PlayerMap l r) = case k of
    Left  -> Maybe.fromStrict l
    Right -> Maybe.fromStrict r
  adjust f k (PlayerMap l r) = case k of
    Left  -> PlayerMap (f <$> l) r
    Right -> PlayerMap l (f <$> r)
  delete k (PlayerMap l r) = case k of
    Left  -> PlayerMap Strict.Nothing r
    Right -> PlayerMap l Strict.Nothing
  null = \case
    PlayerMap Strict.Nothing Strict.Nothing -> True
    PlayerMap _ _ -> False
  (!) (PlayerMap l r) = \case
    Left -> Strict.fromJust l
    Right -> Strict.fromJust r
  mapWithKey f (PlayerMap l r) = PlayerMap (f Left <$> l) (f Right <$> r)
  map = fmap
  size = length . elems
  toList (PlayerMap l r) = Strict.catMaybes [(Left,) <$> l, (Right,) <$> r]
  elems (PlayerMap l r) = Strict.catMaybes [l, r]
  unionWith f (PlayerMap l1 r1) (PlayerMap l2 r2) =
    PlayerMap (Strict.Maybe.unionWith f l1 l2) (Strict.Maybe.unionWith f r1 r2)
  intersectionWith f (PlayerMap l1 r1) (PlayerMap l2 r2) =
    PlayerMap (f <$> l1 <*> l2) (f <$> r1 <*> r2)
  singleton k v = case k of
    Left  -> PlayerMap (Strict.Just v) Strict.Nothing
    Right -> PlayerMap Strict.Nothing (Strict.Just v)
  insertWith f k v (PlayerMap l r) = case k of
    Left  -> PlayerMap (Strict.Just $ Strict.maybe v (f v) l) r
    Right -> PlayerMap l (Strict.Just $ Strict.maybe v (f v) r)
  insert k v (PlayerMap l r) = case k of
    Left  -> PlayerMap (Strict.Just v) r
    Right -> PlayerMap l (Strict.Just v)
  empty = PlayerMap Strict.Nothing Strict.Nothing
  fromListWith f = foldl' (flip $ uncurry $ insertWith f) empty
  fromList = foldl' (flip $ uncurry insert) empty

instance Vector a => Vector (PlayerMap a) where
  scale = genericScaleMap
  add = genericAddMap
  vnegate = genericVNegateMap
  zero = genericZeroMap
  vsum = genericVSumMap

instance KeyTraversable PlayerMap where
  traverseWithKey func (PlayerMap l r) =
    PlayerMap <$>
      Strict.maybe (pure Strict.Nothing) (fmap Strict.Just . func Left) l <*>
      Strict.maybe (pure Strict.Nothing) (fmap Strict.Just . func Right) r

initPlayerMap :: (PlayerIndex -> Maybe a) -> PlayerMap a
initPlayerMap playerOp =
  PlayerMap (Maybe.toStrict $ playerOp Left) (Maybe.toStrict $ playerOp Right)

playerList :: [PlayerIndex]
playerList = [Left ..]

opposite :: PlayerIndex -> PlayerIndex
opposite = \case {Left -> Right; Right -> Left}
