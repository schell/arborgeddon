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

concatTime :: Double -> State Game ()
concatTime t = do
    l <- use frameTimes
    p <- use timeNow
    let ll = length l
        dt = t - p
    timePrev   .= p
    timeNow    .= t
    frameTimes .= take ll (dt:l)
    avgFPS     .= 1/(sum l /fromIntegral ll)
