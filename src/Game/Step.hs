module Game.Step where

import Game.Game
import Game.Types

import Control.Lens
import Control.Monad.State

step :: Game -> Game
step = execState $ do
    n <- use timeNow
    p <- use timePrev
    s <- use scene
    scene .= tickSprite (n - p) s
