module Main where

import Game.Regret (defaultMain)

import Game.RPS (RPS(RPS))

main :: IO ()
main = defaultMain (pure RPS)
