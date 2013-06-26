{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies, RecordWildCards, FlexibleInstances, TypeSynonymInstances #-}
module Game.Types where

import Geometry

import Control.Lens
import Control.Monad.State
import Data.Typeable

import Data.List                    ( intercalate )

import Graphics.UI.GLFW as GLFW

import Graphics.Rendering.OpenGL hiding ( Matrix )

-- | A rect at (x,y) with (width,height).
data Rectangle = Rectangle Vec2 Vec2 deriving (Show, Eq)

{- Sprites -}
data Sprite2d a = Sprite2d { _spriteRsrc :: Maybe a
                           , _spriteFPS  :: Double
                           , _spriteTick :: Double
                           , _frame      :: Int
                           , _boxes      :: [Rectangle]
                           , _rsrcName   :: String
                           } deriving (Show, Eq)

makeLenses ''Sprite2d

{- Rendering Typeclass -}
class Renderable a where
    render  :: a -> IO a

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

{- Lenses -}
makeLenses ''DisplayElement
makeLenses ''InputState
makeLenses ''SavedGameState


translateRect :: Float -> Float -> Rectangle -> Rectangle
translateRect x y (Rectangle (x',y') (w,h)) = Rectangle (x'+x, y'+y) (w,h)

-- | Increments a sprite's tick.
tickSprite :: (Show a, Eq a)
           => Double     -- ^ The change in seconds since the last call.
           -> Sprite2d a -- ^ The sprite to update.
           -> Sprite2d a
tickSprite dt sprite =
    let tick    = sprite^.spriteTick         -- Leftover time from last tick
        fps     = (sprite^.spriteFPS)          -- Frames per second
        total   = tick + dt                  -- Total time elapsed, leftover + dt
        finc    = total * fps                -- frames elapsed
        fint    = floor finc :: Int
        tick'   = (finc - fromIntegral fint) / fps-- leftover time this tick
        frame'  = sprite^.frame + fint
        nFrames = length (sprite^.boxes)
        frame'' = if nFrames == 0
                  then frame'
                  else frame' `mod` nFrames
    in flip execState sprite $ do
        frame .= frame''
        spriteTick .= tick'

showSprite :: Sprite2d a -> String
showSprite s = "Sprite2d{" ++ intercalate ", " [ "spriteFPS = "  ++ show (s^.spriteFPS)
                                               --, "spriteTick = " ++ show (s^.spriteTick)
                                               , "frame = "      ++ show (s^.frame)
                                               --, "boxes = "      ++ show (s^.boxes)
                                               ] ++ "}"

instance Renderable (Sprite2d String) where
    render s = do
        putStrLn $ showSprite s
        return s

instance Renderable (Sprite2d TextureObject) where
    render s = do
        case s^.spriteRsrc of
             Nothing -> putStrLn $ showSprite s
             _       -> putStrLn "Sprite has a tex object."
        return s


