{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies, RecordWildCards #-}
module Game.Types where

import Data.Typeable
import Geometry.Types
import Graphics.Vbo
import Graphics.Rendering.OpenGL
import Control.Lens
import Graphics.UI.GLFW             ( Key, MouseButton )


{- Game States -}
data GameState a = GameState { _scene      :: DisplayElement a
                             , _timeNow    :: Double
                             , _timePrev   :: Double
                             , _input      :: InputState
                             , _events     :: [InputEvent]
                             , _renderSrcs :: RenderSources
                             } deriving (Show, Eq, Typeable)

type RenderSources = (TextureObject, InterleavedVbo)
type Game = GameState GLfloat
type SavedGame = SavedGameState GLfloat

data DisplayElement a = DisplayElement { _children  :: [DisplayElement a]
                                       , _transform :: Transform3d a
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

{- Typeclass Animation -}
class Animation a where
    tick         :: Double -> a -> a
    currentFrame :: a -> Int

{- Sprites -}
data Sprite2d a = Sprite2d { _spriteRenderable :: a
                           , _frame            :: Int
                           , _frameBoxes       :: [Rectangle]
                           }

-- | A rect at (x,y) with (width,height).
data Rectangle = Rectangle Vec2 Vec2 deriving (Show, Eq)

{- Lenses -}
makeLenses ''GameState
makeLenses ''DisplayElement
makeLenses ''InputState
makeLenses ''SavedGameState

{- Getters, Mutators -}
rotation :: Lens' (DisplayElement a) (Rotation3d a)
rotation = transform._1

translation :: Lens' (DisplayElement a) (Translation3d a)
translation = transform._3

scale :: Lens' (DisplayElement a) (Scale3d a)
scale = transform._2

