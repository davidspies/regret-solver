{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Game.Regret.Internal (playouts) where

import Control.Monad (forM, forM_, join, replicateM_)
import Control.Monad.Random (MonadRandom, MonadSTRandom)
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
import Game.Regret.Options.Class (selectChance, selectOpposite, selectSelf)
import Game.Regret.Options.SelectionPath (SelectionPath(..), noPaths, nullPaths)
import Game.Regret.Options.SelectionPath.Map (SelectionMap)
import qualified Game.Regret.Options.SelectionPath.Map as S

data IndividualResult am v = MyTurn (am v) | NotMyTurn v

type RG g = Regret (InfoMap g) (Node (ActionMap g))
type TRG g = TopRegret (InfoMap g) (Node (ActionMap g))

fetchPolicy :: Game g => g -> InfoSet g -> RG g (Dist (Action g))
fetchPolicy g info = fromMaybe startNode <$> Monad.regretValue info
  where
    startNode = normalize $ Node $ Map.map (const 0) $ getActions g info

getInfos :: Game g => g -> Game.State g -> PlayerMap (InfoSet g)
getInfos g curState = initPlayerMap $ \i -> getInfoSet g i curState

probe :: Game g => g -> Game.State g -> RG g (Value g)
probe g curState = case getPrimitiveValue g curState of
  Just val -> return val
  Nothing  -> do
    let infosets = getInfos g curState
    policies <- forM infosets $ fetchPolicy g
    traverseSelect policies $ \selections ->
      renderSelection (applyActions g selections curState) $ probe g

selectPlayerActions :: (Map a, Map.MapValue a (), Map.MapValue a (Int, Float), MonadSTRandom m)
  => a () -> SelectionPath -> m (SelectionMap a)
selectPlayerActions _acts _path = undefined

outcomes :: forall g. Game g
  => g
  -> PlayerIndex
  -> SelectionPath
  -> Game.State g
  -> PlayerMap (Dist (Action g))
  -> RG g (Value g)
outcomes g p path curState policies =
    case getActions g <$> getInfoSet g p curState of
      Nothing   -> selectOpposite path (sequence policies) doActions
      Just acts -> do
        let selfStrat = policies Map.! p
            remaining = Map.delete p policies
            ifVisited newpath chosen = do
              v <- selectOpposite newpath (Map.insert p chosen <$> sequence remaining) doActions
              -- processRegrets g p _ selfStrat v
              return v
            ifUnvisited chosen = do
              s <- sample $ do
                ma <- sequence remaining
                applyActions g (Map.insert p chosen ma) curState
              probe g s
        (selected, res) <- selectSelf path selfStrat ifVisited ifUnvisited
        undefined -- TODO Something
        return res

        -- selected <- selectPlayerActions acts path
        -- let allActs = S.union selected (S.map (const (noPaths, 0)) acts)
        -- fmap MyTurn $ S.forMWithKey allActs $ \a (newpath, probScale) ->
          -- if nullPaths newpath
          --   then renderSelection (applyActions g (Map.insert p a selections) curState) $ probe g
          --   else scaleBy (1 / probScale) (doActions (Map.insert p a selections) newpath)
  where
    doActions :: SelectionPath -> PlayerMap (Action g) -> RG g (Value g)
    doActions newpath ma = selectChance newpath (applyActions g ma curState) (playout g p)

renderSelection :: (Vector b, MonadRandom m) => Dist a -> (a -> m b) -> m b
renderSelection x continuation = sample x >>= continuation

traverseSelect :: (Traversable f, Vector b, MonadRandom m) => f (Dist a) -> (f a -> m b) -> m b
traverseSelect = renderSelection . sequence

processRegrets :: Game g
  => g -> PlayerIndex -> InfoSet g -> Dist (Action g) -> ActionMap g (Value g)
  -> RG g (Value g)
processRegrets g p info myPolicy r = do
  let ut = getUtility g p
      ev = expected $ (r Map.!) <$> myPolicy
      utev = ut ev
  Monad.addRegret info $ Node $ Map.map (\v -> ut v - utev) r
  return ev

playout :: Game g => g -> PlayerIndex -> SelectionPath -> Game.State g -> RG g (Value g)
playout g p path curState =
  case getPrimitiveValue g curState of
    Just val -> return val
    Nothing -> do
      let infosets = getInfos g curState
      policies <- forM infosets $ fetchPolicy g
      outcomes g p path curState policies

newtype Node m = Node (m Float)

instance (Map.MapValue m Float, Map m) => Vector (Node m) where
  scale c (Node m) = Node $ Vector.genericScaleMap c m
  add (Node v) (Node w) = Node $ Vector.genericAddMap v w
  zero = Node Vector.genericZeroMap
  vnegate (Node m) = Node $ Vector.genericVNegateMap m
  vsum = Node . Vector.genericVSumMap . map (\(Node m) -> m)

instance (Map.MapValue m Float, Map m) => Normalizing (Node m) where
  type Normal (Node m) = Dist (Key m)
  normalize (Node m) = Dist.normalize $ NonEmpty.fromList $ map Tuple.swap $ Map.toList m
  forget = Node . Map.fromList . map Tuple.swap . NonEmpty.toList . Dist.pieces
  untypedNormalize (Node m)
    | Map.null m = error "Empty map"
    | otherwise = Node $
        Map.map (if tot <= 0
          then const (1 / fromIntegral (Map.size m))
          else (\x -> if x <= 0 then 0 else x / tot)
        ) m
    where
      tot = foldl' (\v x -> if x <= 0 then v else v + x) 0 (Map.elems m)

playouts :: Game g => g -> Int -> Int -> TRG g ()
playouts g count npaths = do
  let start = startState g
  replicateM_ count $
    forM_ playerList $ \p ->
      Monad.saveRegrets_ $ playout g p (SelectionPath npaths) start
