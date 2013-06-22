{-# LANGUAGE DeriveDataTypeable, TypeFamilies, RecordWildCards #-}
module Game.Types where

import Data.Typeable
import Geometry.Types

import Graphics.UI.GLFW ( Key, MouseButton )

{- Game States -}
data GameState a = GameState { _scene      :: DisplayElement a
                             , _timeNow    :: Double
                             , _timePrev   :: Double
                             , _input      :: InputState
                             , _events     :: [InputEvent]
                             } deriving (Show, Eq, Typeable)

data DisplayElement a = DisplayElement { _children  :: [DisplayElement a]
                                       , _transform :: Transform3d a
                                       , _render    :: Matrix a -> IO ()
                                       }

instance Show a => Show (DisplayElement a) where
    show DisplayElement{..} = "DisplayElement{_children = "++show _children++", _transform = "++show _transform++"}"

instance Eq a => Eq (DisplayElement a) where
    a == b = (_children a == _children b) && (_transform a == _transform b)

data InputState = InputState { _keysPressed         :: [Key]
                             , _mousePosition       :: (Int, Int)
                             , _mouseButtonsPressed :: [MouseButton]
                             } deriving (Show, Eq, Typeable)

data SavedGameState a = SavedGameState { __ :: String
                                       } deriving (Show, Eq, Typeable)

{- Events -}
data InputEvent = KeyButtonDown Key
                | KeyButtonUp Key
                | MouseButtonDown MouseButton
                | MouseButtonUp MouseButton
                | MouseMovedTo (Int, Int)
                deriving (Show, Eq)

