module Game.Sprites where

import Game.Types

scooterSprite :: Sprite2d a
scooterSprite = Sprite2d Nothing 12.0 0.0 0 scooterBoxes "scooter"
    where scooterBoxes = [ Rectangle (x*fWidth,y*fWidth) (fWidth,fWidth) | y <- [0..3], x <- [0..4] ]
          fWidth        = 32-- /256
