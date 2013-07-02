module Game.Sprites where

import Graphics.Sprite

scooterSprite :: Sprite2d a
scooterSprite = Sprite2d Nothing 12.0 0.0 0 scooterBoxes "scooter"
    where scooterBoxes = makeFrames 256 256 32 32 4 3

