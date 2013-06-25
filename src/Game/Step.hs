module Game.Step where

import Game.Types
import Game.State
import Geometry

import Control.Lens
import Control.Monad.State

step :: (Num a, Fractional a) => GameState a -> GameState a
step = execState $ do
    n <- use timeNow
    p <- use timePrev
    s <- use scene
    scene .= tickSprite (n - p) s
