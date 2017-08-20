{-# LANGUAGE TypeFamilies #-}

module Data.Map.Mutable.Generic where

import Control.Monad.ST (ST)
import Data.Hashable (Hashable)
import qualified Data.HashTable.Class as C
import qualified Data.HashTable.ST.Cuckoo as HTM
import Data.STRef (STRef, modifySTRef, newSTRef, readSTRef)

import qualified Data.Map.Generic as Immutable

type family Key (m :: * -> * -> *)

class Map m where
  lookup :: Key m -> m s v -> ST s (Maybe v)
  insertWith :: (v -> v -> v) -> Key m -> v -> m s v -> ST s ()
  insert :: Key m -> v -> m s v -> ST s ()
  new :: ST s (m s v)
  delete :: Key m -> m s v -> ST s ()
  toList :: m s v -> ST s [(Key m, v)]

newtype HTable k s v = HTable (HTM.HashTable s k v)

type instance Key (HTable k) = k

instance (Eq k, Hashable k) => Map (HTable k) where
  lookup k (HTable h) = HTM.lookup h k
  insertWith join k v (HTable h) = HTM.lookup h k >>= HTM.insert h k . maybe v (join v)
  insert k v (HTable h) = HTM.insert h k v
  new = HTable <$> HTM.new
  delete k (HTable h) = HTM.delete h k
  toList (HTable h) = C.toList h

newtype STMap m s v = STMap (STRef s (m v))

type instance Key (STMap m) = Immutable.Key m

instance Immutable.Map m => Map (STMap m) where
  lookup k (STMap r) = Immutable.lookup k <$> readSTRef r
  insertWith join k v (STMap r) = modifySTRef r $ Immutable.insertWith join k v
  insert k v (STMap r) = modifySTRef r $ Immutable.insert k v
  new = STMap <$> newSTRef Immutable.empty
  delete k (STMap r) = modifySTRef r $ Immutable.delete k
  toList (STMap r) = Immutable.toList <$> readSTRef r
