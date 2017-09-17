{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Map.Mutable.Generic
    ( Delete (..)
    , HTable
    , Insert (..)
    , InsertWith (..)
    , Lookup (..)
    , Key
    , Map (..)
    , STMap
    ) where

import Control.Arrow (first)
import Control.Monad.ST (ST)
import Data.Hashable (Hashable, Hashed, hashed, unhashed)
import qualified Data.HashTable.Class as C
import qualified Data.HashTable.ST.Cuckoo as HTM
import qualified Data.Map.Strict as StrictMap
import Data.STRef (STRef, modifySTRef, newSTRef, readSTRef)
import Prelude hiding (lookup)

import qualified Data.Map.Generic as Immutable

type family Key (m :: * -> * -> *)

newtype Lookup m = Lookup {lookupK' :: forall s v. m s v -> ST s (Maybe v)}
newtype InsertWith m =
  InsertWith {insertKWith' :: forall s v. (v -> v -> v) -> v -> m s v -> ST s ()}
newtype Insert m = Insert {insertK' :: forall s v. v -> m s v -> ST s ()}
newtype Delete m = Delete {deleteK' :: forall s v. m s v -> ST s ()}

class Map m where
  lookup :: Key m -> m s v -> ST s (Maybe v)
  insertWith :: (v -> v -> v) -> Key m -> v -> m s v -> ST s ()
  insert :: Key m -> v -> m s v -> ST s ()
  new :: ST s (m s v)
  delete :: Key m -> m s v -> ST s ()
  toList :: m s v -> ST s [(Key m, v)]
  keyedOperations :: Key m -> (Lookup m, InsertWith m, Insert m, Delete m)
  keyedOperations k =
    (Lookup (lookup k), InsertWith (`insertWith` k), Insert (insert k), Delete (delete k))

newtype HTable k s v = HTable (HTM.HashTable s (Hashed k) v)

type instance Key (HTable k) = k

lookupHashed :: Eq k => Hashed k -> HTable k s v -> ST s (Maybe v)
lookupHashed hk (HTable h) = HTM.lookup h hk

insertHashedWith :: Eq k => (v -> v -> v) -> Hashed k -> v -> HTable k s v -> ST s ()
insertHashedWith join hk v (HTable h) = HTM.lookup h hk >>= HTM.insert h hk . maybe v (join v)

insertHashed :: Eq k => Hashed k -> v -> HTable k s v -> ST s ()
insertHashed hk v (HTable h) = HTM.insert h hk v

deleteHashed :: Eq k => Hashed k -> HTable k s v -> ST s ()
deleteHashed hk (HTable h) = HTM.delete h hk

instance (Eq k, Hashable k) => Map (HTable k) where
  lookup = lookupHashed . hashed
  insertWith join = insertHashedWith join . hashed
  insert = insertHashed . hashed
  new = HTable <$> HTM.new
  delete = deleteHashed . hashed
  toList (HTable h) = map (first unhashed) <$> C.toList h
  keyedOperations k = let hk = hashed k in
    ( Lookup (lookupHashed hk)
    , InsertWith (`insertHashedWith` hk)
    , Insert (insertHashed hk)
    , Delete (deleteHashed hk)
    )

newtype STMap k s v = STMap (STRef s (StrictMap.Map k v))

type instance Key (STMap k) = k

instance Ord k => Map (STMap k) where
  lookup k (STMap r) = Immutable.lookup k <$> readSTRef r
  insertWith join k v (STMap r) = modifySTRef r $ Immutable.insertWith join k v
  insert k v (STMap r) = modifySTRef r $ Immutable.insert k v
  new = STMap <$> newSTRef Immutable.empty
  delete k (STMap r) = modifySTRef r $ Immutable.delete k
  toList (STMap r) = Immutable.toList <$> readSTRef r
