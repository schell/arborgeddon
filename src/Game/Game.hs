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
import Control.Monad.State
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.OpenGL.GL.Shaders.Program
import Foreign.Ptr                  ( Ptr )
import Foreign.Marshal.Array        ( withArray )
import Foreign.C.String             ( withCString )


type Rsrc = (TextureObject, InterleavedVbo)
type Game = GameState Rsrc
type SavedGame = SavedGameState Rsrc
type Sprite = Sprite2d Rsrc

{- Game States -}
data GameState a = GameState { _scene      :: Sprite2d a
                             , _frameTimes :: [Double]
                             , _avgFPS     :: Double
                             , _windowSize :: (Int, Int)
                             , _timeNow    :: Double
                             , _timePrev   :: Double
                             , _input      :: InputState
                             , _events     :: [InputEvent]
                             , _resources  :: Maybe (M.Map String TextureObject)
                             } deriving (Show, Eq, Typeable)

makeLenses ''GameState

{- Getters, Mutators -}
rotation :: Lens' (DisplayElement a) (Rotation3d a)
rotation = transform._1

translation :: Lens' (DisplayElement a) (Translation3d a)
translation = transform._3

scale :: Lens' (DisplayElement a) (Scale3d a)
scale = transform._2

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

instance Renderable Game where
    render game = do
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
        render $ game^.scene
        GLFW.swapBuffers

    initRsrcs _ game@GameState{} = do
        putStrLn "Initializing resources for game."
        -- Turn on texturing.
        texture Texture2D $= Enabled
        -- Load our textures or die.
        Just t <- loadTexture "/Users/schell/Code/arborgeddon/data/textures/animation-test.png"
        -- Set the texture params on our bound texture.
        textureFilter   Texture2D   $= ((Nearest, Nothing), Nearest)
        textureWrapMode Texture2D S $= (Repeated, Clamp)
        textureWrapMode Texture2D T $= (Repeated, Clamp)

        putStrLn "Giving resources to sprites."
        let texMap = M.insert "scooter" t M.empty
        scene' <- initRsrcs (Just texMap) (game^.scene)
        return $ flip execState game $ do
            scene .= scene'
            resources .= Just texMap

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


