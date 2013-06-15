module Game.Step where

import Game.State
import Geometry

import Control.Lens
import Control.Monad.State

step :: Double -> GameState -> GameState
step time = execState $ do
    t <- use timeNow
    (Rotation x y z) <- use rotation

    timePrev .= t
    timeNow  .= time
    rotation .= Rotation x (y+0.005) (z+0.001)

