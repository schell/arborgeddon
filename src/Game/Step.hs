module Game.Step where

import Game.State ( GameState(..) )
import Geometry
import Control.Lens

step :: GameState -> GameState
step game = let nTransform = ( Rotation x (y+0.001) (z+0.001)
                             , Scale 1 1 1
                             , Translation 0 0 0
                             )
                (Rotation x y z, _,_) = gsTransform game
            in game{ gsTransform = nTransform }

