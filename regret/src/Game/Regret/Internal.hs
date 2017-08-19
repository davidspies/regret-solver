{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Game.Regret.Internal (playouts) where

import Control.Monad (forM, forM_, replicateM, replicateM_)
import Control.Monad.Random (MonadRandom, uniform)
import Data.Foldable (foldl')
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import qualified Data.Tuple as Tuple

import Control.Monad.Scale (scaleBy)
import Data.Dist hiding (normalize)
import qualified Data.Dist as Dist
import Data.Map.Generic (Key, Map)
import qualified Data.Map.Generic as Map
import Data.Normalizing (Normalizing(..))
import Data.Vector.Class (Vector(..))
import qualified Data.Vector.Class as Vector
import Game
import Game.PlayerMap (PlayerIndex, PlayerMap, initPlayerMap, playerList)
import Game.Regret.Monad (Regret, TopRegret)
import qualified Game.Regret.Monad as Monad

data IndividualResult am v = MyTurn (am v) | NotMyTurn v

type RG g = Regret (InfoMap g) (Node (ActionMap g))
type TRG g = TopRegret (InfoMap g) (Node (ActionMap g))

fetchPolicy :: Game g => g -> InfoSet g -> RG g (Dist (Action g))
fetchPolicy g info = fromMaybe startNode <$> Monad.regretValue info
  where
    startNode = normalize $ Node $ const 0 <$> getActions g info

getInfos :: Game g => g -> Game.State g -> PlayerMap (InfoSet g)
getInfos g curState = initPlayerMap (getNumPlayers g) $ \i -> getInfoSet g i curState

probe :: Game g => g -> Game.State g -> RG g (Value g)
probe g curState = case getPrimitiveValue g curState of
  Just val -> return val
  Nothing  -> do
    let infosets = getInfos g curState
    policies <- forM infosets $ fetchPolicy g
    selections <- mapM sample policies
    sample (applyActions g selections curState) >>= probe g

selectPlayerActions :: (Map a, MonadRandom m)
  => a () -> SelectionPath -> m (a (SelectionPath, Double))
selectPlayerActions acts (SelectionPath npaths) = do
  let probScale = fromIntegral npaths / fromIntegral (Map.size acts)
  fmap ((, probScale) . SelectionPath) . Map.fromListWith (+) . map (, 1) <$>
    replicateM npaths (uniform $ Map.keys acts)

outcomes :: Game g
  => g -> PlayerIndex -> SelectionPath -> Game.State g -> PlayerMap (Dist (Action g))
  -> RG g (IndividualResult (ActionMap g) (Value g))
outcomes g p path curState policies = do
    selections <- mapM sample policies
    case getActions g <$> getInfoSet g p curState of
      Nothing   -> NotMyTurn <$> doActions selections path
      Just acts -> do
        selected <- selectPlayerActions acts path
        let chosenActions = Map.mapWithKey ((. Just) . (,)) selected
            otherActions  = Map.mapWithKey (const . (, Nothing)) acts
        fmap MyTurn $ forM (Map.union chosenActions otherActions) $ \(a, xs) ->
          case xs of
            Just (newpath, probScale) ->
              scaleBy (1 / probScale) (doActions (Map.insert p a selections) newpath)
            Nothing -> sample (applyActions g (Map.insert p a selections) curState) >>= probe g
  where
    doActions ma newpath = sample (applyActions g ma curState) >>= playout g p newpath

processRegrets :: Game g
  => g -> PlayerIndex -> InfoSet g -> Dist (Action g) -> ActionMap g (Value g)
  -> RG g (Value g)
processRegrets g p info myPolicy r = do
  let ut = getUtility g p
      ev = expected $ (r Map.!) <$> myPolicy
      utev = ut ev
  Monad.addRegret info $ Node $ (\v -> ut v - utev) <$> r
  return ev

playout :: Game g => g -> PlayerIndex -> SelectionPath -> Game.State g -> RG g (Value g)
playout g p path curState =
  case getPrimitiveValue g curState of
    Just val -> return val
    Nothing -> do
      let infosets = getInfos g curState
      policies <- forM infosets $ fetchPolicy g
      outcomes g p path curState policies >>= \case
        MyTurn v    -> processRegrets g p (infosets Map.! p) (policies Map.! p) v
        NotMyTurn v -> return v

newtype Node m = Node (m Double)
newtype SelectionPath = SelectionPath Int

deriving instance Show (m Double) => Show (Node m)

instance Map m => Vector (Node m) where
  scale c (Node m) = Node $ Vector.genericScaleMap c m
  add (Node v) (Node w) = Node $ Vector.genericAddMap v w
  zero = Node Vector.genericZeroMap
  vnegate (Node m) = Node $ Vector.genericVNegateMap m
  vsum = Node . Vector.genericVSumMap . map (\(Node m) -> m)

instance Map m => Normalizing (Node m) where
  type Normal (Node m) = Dist (Key m)
  normalize (Node m) = Dist.normalize $ NonEmpty.fromList $ map Tuple.swap $ Map.toList m
  forget = Node . Map.fromList . map Tuple.swap . Dist.pieces
  untypedNormalize (Node m)
    | Map.null m = error "Empty map"
    | otherwise = Node $
        fmap (if tot <= 0
          then const (1 / fromIntegral (Map.size m))
          else (\x -> if x <= 0 then 0 else x / tot)
        ) m
    where
      tot = foldl' (\v x -> if x <= 0 then v else v + x) 0 m

playouts :: Game g => g -> Int -> Int -> TRG g ()
playouts g count npaths = do
  let start = startState g
  replicateM_ count $
    forM_ (playerList (getNumPlayers g)) $ \p ->
      Monad.saveRegrets_ $ playout g p (SelectionPath npaths) start
