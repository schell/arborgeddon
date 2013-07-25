module Main where

import App
import Game.Game

main :: IO ()
main = do
    app <- initializeApp loadGame
    startApp app

