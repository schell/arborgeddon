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

instance Renderable Game where
    render game = do
        let (wi,hi) = game^.windowSize
            (w,h) = (fromIntegral wi, fromIntegral hi)
            mId   = identityN 4 :: Matrix GLfloat
            p     = orthoMatrix 0 w 0 h 0 1
            ups   = [matUpdate, matUpdate, samUpdate]
            names = ["projection","modelview","sampler"]
            arrs  = map concat [p, mId, []]
            matUpdate loc   = glUniformMatrix4fv loc 1 1
            samUpdate loc _ = glUniform1i loc 0
        -- Clear the screen and the depth buffer.
        clear [ColorBuffer, DepthBuffer]
        -- Set the viewport.
        viewport $= (Position 0 0, Size (fromIntegral wi) (fromIntegral hi))
        print $ game^.windowSize
        -- Update the matrix uniforms.
        updateUniforms names ups arrs
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


