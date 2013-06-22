module Game.Step where

import Game.Types
import Game.State
import Geometry

import Control.Lens
import Control.Monad.State

step :: (Num a, Fractional a) => GameState a -> GameState a
step = execState $ do
    (Rotation x y z) <- use $ scene.rotation
    scene.rotation .= Rotation x (y+0.005) (z+0.001)
