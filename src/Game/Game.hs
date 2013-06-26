{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeSynonymInstances, FlexibleInstances #-}
module Game.Game where

import Game.Types
import Graphics
import Geometry

import qualified Data.Map as M
import qualified Graphics.UI.GLFW as GLFW

import Control.Lens              hiding ( transform )
import Graphics.Rendering.OpenGL hiding ( Matrix ) 

import Data.Typeable
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.OpenGL.GL.Shaders.Program
import Data.Maybe                   ( isNothing )
import Foreign.Ptr                  ( Ptr )
import Foreign.Marshal.Array        ( withArray )
import Foreign.C.String             ( withCString )


type RenderSource = (TextureObject, InterleavedVbo)
type Rsrc = TextureObject
type Game = GameState Rsrc
type SavedGame = SavedGameState Rsrc
type Sprite = Sprite2d Rsrc

{- Game States -}
data GameState a = GameState { _scene      :: Sprite2d a
                             , _timeNow    :: Double
                             , _timePrev   :: Double
                             , _input      :: InputState
                             , _events     :: [InputEvent]
                             , _resources  :: Maybe (M.Map String a)
                             } deriving (Show, Eq, Typeable)

makeLenses ''GameState

{- Getters, Mutators -}
rotation :: Lens' (DisplayElement a) (Rotation3d a)
rotation = transform._1

translation :: Lens' (DisplayElement a) (Translation3d a)
translation = transform._3

scale :: Lens' (DisplayElement a) (Scale3d a)
scale = transform._2

instance Renderable Game where
    render game = do
        -- Make sure we have resources to render.
        game' <- if isNothing $ game^.resources
                 then initRsrcs game
                 else return game
        -- Clear the screen and the depth buffer.
        clear [ColorBuffer, DepthBuffer]
        -- Update the matrix uniforms.
        let --t          = game^.scene.transform
            mv         = identityN 4--applyTransformation t $ identityN 4
            p          = identityN 4
        -- Texture
        --activeTexture     $= TextureUnit 0
        --textureBinding Texture2D $= Just tex
        --printError

        updateMatrixUniforms p mv
        -- Geometry
        --bindInterleavedVbo ivbo
        --drawArrays Triangles 0 6
        --printError
        scene' <- render $ game'^.scene
        GLFW.swapBuffers
        return game'{_scene = scene'}

initRsrcs :: Game -> IO Game
initRsrcs game = do
    -- Turn on texturing.
    texture Texture2D $= Enabled
    -- Load our textures or die.
    Just t <- loadTexture "/Users/schell/Code/arborgeddon/data/textures/animation-test.png"
    -- Set the texture params on our bound texture.
    textureFilter   Texture2D   $= ((Nearest, Nothing), Nearest)
    textureWrapMode Texture2D S $= (Repeated, Clamp)
    textureWrapMode Texture2D T $= (Repeated, Clamp)
    -- Vertex data things.
    -- let vertexData = square :: [GLfloat]
    --     texData    = [ 0.0, 0.0
    --                  , 0.0, 1.0
    --                  , 1.0, 1.0
    --                  , 1.0, 1.0
    --                  , 0.0, 0.0
    --                  , 1.0, 0.0
    --                  ]
    -- ivbo  <- interleavedVbo [vertexData, texData] [3,2] [AttribLocation 0, AttribLocation 1]
    -- putStrLn $ "Got ivbo "++show ivbo

    -- Set the game's resources.
    let texMap = M.insert "scooter" t M.empty
        game'  = game{_resources = Just texMap}
    print game'
    return game'



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


