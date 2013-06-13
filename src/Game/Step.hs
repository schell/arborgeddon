module Game.Step where

import Game.State
import Geometry

import Control.Lens
import Control.Monad.State (State, execState, get)

step :: GameState -> GameState
step = execState $ do

