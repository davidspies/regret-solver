module Data.Strict.Maybe.Util
    ( catMaybes
    , unionWith
    ) where

import qualified Data.Maybe as Unstrict
import Data.Strict.Maybe
import Prelude hiding (Maybe(..), maybe)

catMaybes :: [Maybe a] -> [a]
catMaybes = Unstrict.mapMaybe (maybe Unstrict.Nothing Unstrict.Just)

unionWith :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
unionWith op x = maybe x (Just . maybe id op x)
