{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies, RecordWildCards, FlexibleInstances, TypeSynonymInstances #-}
module Game.Types where

import Geometry
import Graphics

import Control.Lens
import Control.Monad.State
import Data.Typeable

import Data.List                    ( intercalate )

import Graphics.UI.GLFW     as GLFW
import qualified Data.Map   as M

import Graphics.Rendering.OpenGL hiding ( Matrix )

-- | A rect at (x,y) with (width,height).
data Rectangle a = Rectangle (Vec2 a) (Vec2 a) deriving (Show, Eq)

toTriangleRect :: Rectangle GLfloat -> [GLfloat]
toTriangleRect (Rectangle (x,y) (w,h)) = concat [tl, bl, br, br, tl, tr]
    where tl = [x,y]
          tr = [x+w,y]
          bl = [x,y+h]
          br = [x+w,y+h]

toTriangles :: [Rectangle GLfloat] -> [GLfloat]
toTriangles = concatMap toTriangleRect

{- Sprites -}
data Sprite2d a = Sprite2d { _spriteRsrc :: Maybe a
                           , _spriteFPS  :: Double
                           , _spriteTick :: Double
                           , _frame      :: Int
                           , _boxes      :: [Rectangle GLfloat]
                           , _rsrcName   :: String
                           } deriving (Show, Eq)

makeLenses ''Sprite2d

type TextureMap = M.Map String TextureObject
{- Rendering Typeclass -}
class Renderable a where
    initRsrcs :: Maybe TextureMap -> a -> IO a
    initRsrcs _ = return . id
    render    :: a -> IO ()

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
    initRsrcs _ = return . id
    render      = putStrLn . showSprite

instance Renderable (Sprite2d TextureObject) where
    initRsrcs (Just m) s = return $
        case M.lookup (s^.rsrcName) m of
            Nothing -> s
            Just t  -> execState (spriteRsrc .= Just t) s
    initRsrcs _ s        = return s

    render s = case s^.spriteRsrc of
                   Nothing -> putStrLn $ showSprite s
                   _       -> putStrLn "Sprite has a tex object."


instance Renderable (Sprite2d (TextureObject, InterleavedVbo)) where
    initRsrcs (Just m) s = case M.lookup (s^.rsrcName) m of
        Nothing -> return s
        Just t  -> do
            let comps  = [3,2]
                attrbs = [AttribLocation 0, AttribLocation 1]
                boxs   = s^.boxes
                mScl   = scaleMatrix3d 32 32 1 :: Matrix GLfloat
                verts  = concat $ replicate (length boxs) $ transformVector3 mScl texSquare3d
                uvs    = toTriangles boxs
                datas  = [verts, uvs]
            ivbo <- interleavedVbo datas comps attrbs
            return s{_spriteRsrc = Just (t, ivbo)}

    initRsrcs _ s        = return s

    render s = case s^.spriteRsrc of
                   Nothing   -> putStrLn $ showSprite s
                   Just (t, ivbo) -> do
                       activeTexture $= TextureUnit 0
                       textureBinding Texture2D $= Just t
                       printError
                       bindInterleavedVbo ivbo
                       let frame' = s^.frame
                       drawArrays Triangles (fromIntegral frame'*6) 6

