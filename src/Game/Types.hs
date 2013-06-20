{-# LANGUAGE DeriveDataTypeable, TypeFamilies #-}
module Game.Types where

import Graphics.Rendering.OpenGL.Raw
import Data.Typeable
import Geometry.Types

import Graphics.UI.GLFW ( Key, MouseButton )

{- Game States -}
data GameState = GameState { _transform  :: Transform3d GLfloat
                           , _timeNow    :: Double
                           , _timePrev   :: Double
                           , _input      :: InputState
                           , _events     :: [InputEvent]
                           } deriving (Show, Eq, Typeable)

data InputState = InputState { _keysPressed         :: [Key]
                             , _mousePosition       :: (Int, Int)
                             , _mouseButtonsPressed :: [MouseButton]
                             } deriving (Show, Eq, Typeable)

data SavedGameState = SavedGameState { _savedTransform :: Transform3d GLfloat
                                     } deriving (Show, Eq, Typeable)

{- Events -}
data InputEvent = KeyButtonDown Key
                | KeyButtonUp Key
                | MouseButtonDown MouseButton
                | MouseButtonUp MouseButton
                | MouseMovedTo (Int, Int)
                deriving (Show, Eq)

