module Data.Maybe.Util
    ( joinWith
    ) where

joinWith :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
joinWith op x = maybe x (Just . maybe id op x)
