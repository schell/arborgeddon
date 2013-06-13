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
import Foreign.C.Types

import Data.Monoid ( mempty )

data GameState = GameState { gsWindow     :: GameWindow
                           , gsTransform  :: Transform3d GLfloat
                           , gsTimeNow    :: Double
                           , gsTimePrev   :: Double
                           } deriving (Show, Eq, Typeable)

data GameWindow = GameWindow { gwSize       :: (Int, Int)
                             , gwPosition   :: (Int, Int)
                             } deriving (Show, Eq, Typeable)

$(deriveSafeCopy 0 'base ''GameState)
$(deriveSafeCopy 0 'base ''GameWindow)

defaultGameState :: GameState
defaultGameState = GameState defaultGameWindow (mempty :: Transform3d GLfloat) 0 0

defaultGameWindow :: GameWindow
defaultGameWindow = GameWindow (800, 600) (0, 0)

getGameState :: Query GameState GameState
getGameState = ask

saveGameState :: GameState -> Update GameState GameState
saveGameState gs = do
    put gs
    return gs

$(makeAcidic ''GameState ['getGameState, 'saveGameState])

instance SafeCopy CFloat where
    putCopy (CFloat f) = contain $ safePut f
    getCopy = contain $ CFloat <$> safeGet
