module Data.Maybe.Util
    ( fromStrict
    , toStrict
    , unionWith
    ) where

import qualified Data.Strict.Maybe as Strict

unionWith :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
unionWith op x = maybe x (Just . maybe id op x)

fromStrict :: Strict.Maybe a -> Maybe a
fromStrict = Strict.maybe Nothing Just

toStrict :: Maybe a -> Strict.Maybe a
toStrict = maybe Strict.Nothing Strict.Just
