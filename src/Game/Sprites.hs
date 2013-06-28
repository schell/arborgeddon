module Game.Sprites where

import Game.Types

scooterSprite :: Sprite2d a
scooterSprite = Sprite2d Nothing 12.0 0.0 0 scooterBoxes "scooter"
    where scooterBoxes = makeFrames 256 256 32 32 4 3 

makeFrames :: (Enum a, Num a, Fractional a) 
           => a -- ^ Texture width
           -> a -- ^ Texture height 
           -> a -- ^ Frame width 
           -> a -- ^ Frame height 
           -> a -- ^ Number of frames across 
           -> a -- ^ Number of frames down 
           -> [Rectangle a]
makeFrames tW tH fW fH fA fD = [ Rectangle (x*w,y*h) (w,h) | y <- [0..fD], x <- [0..fA] ]
    where w = fW/tW
          h = fH/tH
