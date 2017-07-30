module Main where

import Brick.Main

import Solitaire
import UI (app)


main :: IO ()
main = do
  game <- initGameFull defaultSettings
  finalState <- defaultMain app game
  return ()
