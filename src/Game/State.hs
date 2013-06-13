{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.State where

import Geometry.Transformations

import Data.Acid
import Data.SafeCopy
import Data.Typeable
import Graphics.Rendering.OpenGL.Raw
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Control.Lens
import Foreign.C.Types

import Data.Monoid ( mempty )

data GameState = GameState { _window     :: GameWindow
                           , _transform  :: Transform3d GLfloat
                           , _timeNow    :: Double
                           , _timePrev   :: Double
                           } deriving (Show, Eq, Typeable)

data GameWindow = GameWindow { _size       :: (Int, Int)
                             } deriving (Show, Eq, Typeable)

$(deriveSafeCopy 0 'base ''GameState)
$(deriveSafeCopy 0 'base ''GameWindow)

instance SafeCopy CFloat where
    putCopy (CFloat f) = contain $ safePut f
    getCopy = contain $ CFloat <$> safeGet

defaultGameState :: GameState
defaultGameState = GameState defaultGameWindow (mempty :: Transform3d GLfloat) 0 0

defaultGameWindow :: GameWindow
defaultGameWindow = GameWindow (800, 600)

getGameState :: Query GameState GameState
getGameState = ask

saveGameState :: GameState -> Update GameState GameState
saveGameState gs = do
    put gs
    return gs

$(makeAcidic ''GameState ['getGameState, 'saveGameState])

{- Lenses -}
makeLenses ''GameState
makeLenses ''GameWindow

{- Getters, Mutators -}
rotation :: Lens' GameState (Rotation3d GLfloat)
rotation = transform._1

translation :: Lens' GameState (Translation3d GLfloat)
translation = transform._3

scale :: Lens' GameState (Scale3d GLfloat)
scale = transform._2
