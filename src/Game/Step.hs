module Game.Step where

import Game.Types
import Game.State
import Geometry

import Control.Lens
import Control.Monad.State

step :: GameState -> GameState
step = execState $ do
    (Rotation x y z) <- use rotation
    rotation .= Rotation x (y+0.005) (z+0.001)
