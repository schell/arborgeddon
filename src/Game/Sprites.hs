module Game.Sprites where

import Game.Types

scooterSprite :: Sprite2d a
scooterSprite = Sprite2d Nothing 2.0 0.0 0 scooterBoxes "scooter"
    where scooterBoxes = [ Rectangle (x*32,y*32) (32,32) | y <- [0..3], x <- [0..4] ]
