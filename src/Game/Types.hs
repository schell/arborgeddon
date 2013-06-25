{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies, RecordWildCards, FlexibleInstances, TypeSynonymInstances #-}
module Game.Types where

import Geometry
import Graphics

import Control.Lens
import Control.Monad.State
import Data.Typeable
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.OpenGL.GL.Shaders.Program

import Data.List                    ( intercalate )
import Foreign.Ptr                  ( Ptr )
import Foreign.Marshal.Array        ( withArray )
import Foreign.C.String             ( withCString )

import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL hiding ( Matrix )

-- | A rect at (x,y) with (width,height).
data Rectangle = Rectangle Vec2 Vec2 deriving (Show, Eq)

{- Sprites -}
data (Show a, Eq a) => Sprite2d a = Sprite2d { _spriteRsrc :: a
                                             , _spriteFPS  :: Double
                                             , _spriteTick :: Double
                                             , _frame      :: Int
                                             , _boxes      :: [Rectangle]
                                             } deriving (Show, Eq)
makeLenses ''Sprite2d

{- Rendering Typeclass -}
class Renderable a where
    render :: a -> IO ()

{- Game States -}
data GameState a = GameState { _scene      :: Sprite2d String
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

translateRect :: Float -> Float -> Rectangle -> Rectangle
translateRect x y (Rectangle (x',y') (w,h)) = Rectangle (x'+x, y'+y) (w,h)

-- | Increments a sprite's tick.
tickSprite :: (Show a, Eq a)
           => Double     -- ^ The change in seconds since the last call.
           -> Sprite2d a -- ^ The sprite to update.
           -> Sprite2d a
tickSprite dt sprite =
    let tick   = sprite^.spriteTick         -- Leftover time from last tick
        fps    = (sprite^.spriteFPS)          -- Frames per second
        total  = tick + dt                  -- Total time elapsed, leftover + dt
        finc   = total * fps                -- frames elapsed
        fint   = floor finc :: Int
        tick'  = (finc - fromIntegral fint) * fps-- leftover time this tick
    in flip execState sprite $ do
        -- Figure out how many frames we need to update by.
        frame += fint 
        spriteTick .= tick'

instance Renderable (Sprite2d String) where
    render s = putStrLn $ "Sprite2d{" ++ intercalate ", " [ "spriteRsrc = " ++ s^.spriteRsrc
                                                          , "spriteFPS = "  ++ show (s^.spriteFPS)
                                                          , "spriteTick = " ++ show (s^.spriteTick)
                                                          , "frame = "      ++ show (s^.frame)
                                                          , "boxes = "      ++ show (s^.boxes)
                                                          ] ++ "}"

instance Renderable Game where
    render game = do
        -- Clear the screen and the depth buffer.
        clear [ColorBuffer, DepthBuffer]
        -- Update the matrix uniforms.
        let --t          = game^.scene.transform
            (tex,ivbo) = game^.renderSrcs
            mv         = identityN 4--applyTransformation t $ identityN 4
            p          = identityN 4
        -- Texture
        activeTexture     $= TextureUnit 0
        textureBinding Texture2D $= Just tex
        printError

        updateMatrixUniforms p mv
        -- Geometry
        bindInterleavedVbo ivbo
        drawArrays Triangles 0 6
        printError
        render $ game^.scene
        GLFW.swapBuffers

updateMatrixUniforms :: Matrix GLfloat -> Matrix GLfloat -> IO ()
updateMatrixUniforms projection modelview = do
    p <- getCurrentProgram
    -- Get uniform locations
    projectionLoc <- withCString "projection" $ \ptr ->
        glGetUniformLocation (programID p) ptr
    modelviewLoc <- withCString "modelview" $ \ptr ->
        glGetUniformLocation (programID p) ptr
    samplerLoc <- withCString "sampler" $ \ptr ->
        glGetUniformLocation (programID p) ptr
    printError

    -- Clear matrix uniforms.
    withArray (concat projection) $ \ptr ->
        glUniformMatrix4fv projectionLoc 1 1 ptr
    withArray (concat modelview) $ \ptr ->
        glUniformMatrix4fv modelviewLoc 1 1 ptr
    printError

    -- Clear the texture sampler.
    glUniform1i samplerLoc 0
    printError

updateUniforms :: [String]                        -- ^ A list of uniform names.
               -> [GLint -> Ptr GLfloat -> IO ()] -- ^ A list of update functions.
               -> [[GLfloat]]                     -- ^ A list of uniform arrays.
               -> IO ()
updateUniforms names updates datas = do
    p    <- getCurrentProgram
    locs <- mapM (\name -> do
        loc <- withCString name $ \ptr ->
            glGetUniformLocation (programID p) ptr
        printError
        return loc) names
    sequence_ $ zipWith3 (\upd arr loc -> do
        withArray arr $ upd loc
        printError) updates datas locs


