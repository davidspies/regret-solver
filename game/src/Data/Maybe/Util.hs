module Data.Maybe.Util
    ( unionWith
    ) where

unionWith :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
unionWith op x = maybe x (Just . maybe id op x)
