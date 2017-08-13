module Main where

import Game.Regret (defaultMain)
import Game.Select (SelectGame(SelectGame))

import Game.KuhnPoker (KuhnPoker(KuhnPoker))

main :: IO ()
main = defaultMain $ pure $ SelectGame KuhnPoker
