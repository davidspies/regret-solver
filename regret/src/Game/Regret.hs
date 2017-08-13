{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Game.Regret (defaultMain, module X) where

import Control.Monad (forM_)
import Data.Monoid ((<>))
import Options.Applicative
import System.Random (newStdGen)

import qualified Data.Dist as Dist
import Game (Action, Game, InfoSet)
import Game.Regret.Internal as X
import Game.Regret.Monad (runRegret)

data SolverOptions g = SolverOptions
  { numPlayouts        :: Int
  , numPlayoutBranches :: Int
  , game               :: g
  }

solverOptions :: Parser g -> ParserInfo (SolverOptions g)
solverOptions parser = info (go <**> helper) fullDesc
  where
    go = (\numPlayouts numPlayoutBranches game -> SolverOptions{..})
      <$> option auto
        (  long "num-playouts"
        <> short 'n'
        <> help "Number of playouts to run"
        <> metavar "INT"
        )
      <*> option auto
        (  long "num-playout-branches"
        <> short 'b'
        <> help "Number of branches per playout"
        <> metavar "INT"
        <> showDefault
        <> value 10
        )
      <*> parser

defaultMain :: (Game g, Show (Action g), Show (InfoSet g)) => Parser g -> IO ()
defaultMain parser = do
  SolverOptions{..} <- execParser (solverOptions parser)
  randSource <- newStdGen
  let place = runRegret randSource $ playouts game numPlayouts numPlayoutBranches
  forM_ place $ \(k, v) -> do
    print k
    print $ Dist.pieces v
