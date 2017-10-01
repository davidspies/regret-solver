{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Monoid ((<>))
import Options.Applicative

import Game.Regret (defaultMain)
import Game.Select (SelectGame(SelectGame))

import Game.Dudo (Dudo(..))

main :: IO ()
main = defaultMain $
  (\dieSides -> SelectGame Dudo{..})
    <$> option auto
      (  long "die-sides"
      <> help "Number of sides on the die"
      <> metavar "INT"
      <> showDefault
      <> value 6
      )
