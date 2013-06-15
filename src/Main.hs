module Main where

import Graphics
import Geometry
import Game
import Game.Clock

import Data.IORef
import Foreign.Ptr
import Data.Acid
import System.CPUTime
import Control.Monad.State
import Control.Lens hiding ( transform )

import Data.Acid.Local               ( createCheckpointAndClose )
import Data.Acid.Advanced            ( query', update' )
import Data.Maybe                    ( fromMaybe )
import Control.Exception             ( bracket )
import Control.Monad.IO.Class        ( liftIO )
import Control.Monad                 ( void, unless, forever, zipWithM_ )
import Control.Monad.Trans.Class     ( lift )
import System.Exit                   ( exitSuccess )
import System.Directory              ( getCurrentDirectory )
import System.FilePath               ( (</>) )
import Foreign.Marshal.Array         ( withArray )
import Foreign.C.String              ( withCString )
import Graphics.Rendering.OpenGL.Raw ( glGetUniformLocation, glUniformMatrix4fv )

import Graphics.Rendering.OpenGL hiding ( Matrix )
import Graphics.Rendering.OpenGL.GL.Shaders.Program
import qualified Graphics.UI.GLFW   as GLFW

type DisplayState = InterleavedVbo

main :: IO ()
main = do
    putStrLn "Starting Arborgeddon..."
    bracket (openLocalState defaultGameState)
            createCheckpointAndClose
            runGame

runGame :: AcidState GameState -> IO ()
runGame acid = do
    savedGame <- query' acid GetGameState
    gameRef   <- newIORef savedGame
    True      <- initGLFW acid gameRef
    True      <- initShaders

    -- Vertex data things.
    ivbo <- interleavedVbo [vertexData, colorData] [3,4] [AttribLocation 0, AttribLocation 1]

    -- Register our scene drawing function.
    --GLFW.setWindowRefreshCallback windowRefresh
    -- Register our resize window function.
    GLFW.setWindowSizeCallback (\w h -> do
        viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
        modifyIORef gameRef $ execState (window.size .= (w, h))
        stepGame gameRef ivbo)


    forever $ stepGame gameRef ivbo

stepGame :: IORef GameState -> InterleavedVbo -> IO ()
stepGame gameRef ivbo = do
    game    <- readIORef gameRef
    newGame <- updateGameState game
    writeIORef gameRef newGame
    drawScene newGame ivbo

updateGameState :: GameState -> IO GameState
updateGameState gamePrev = do
    newTime <- getTime
    return $ step newTime gamePrev

drawScene :: GameState -> DisplayState -> IO ()
drawScene game ivbo = do
    -- Clear the screen and the depth buffer.
    clear [ColorBuffer, DepthBuffer]
    -- Update the matrix uniforms.
    let t  = game^.transform
        mv = applyTransformation t $ identityN 4
        p  = identityN 4
    updateMatrixUniforms p mv
    bindInterleavedVbo ivbo
    drawArrays Triangles 0 3
    printError
    GLFW.swapBuffers

initGLFW :: AcidState GameState -> IORef GameState -> IO Bool
initGLFW acid gameRef = do
    putStrLn "Initializing the OpenGL window and context."

    True <- GLFW.initialize
    game <- readIORef gameRef
    -- Initialize the window.
    let displayOps = displayOptions{ GLFW.displayOptions_width  = game^.window.size._1
                                   , GLFW.displayOptions_height = game^.window.size._2
                                   }
    True <- GLFW.openWindow displayOps
    -- Make sure the window is gpu'able.
    True <- GLFW.windowIsHardwareAccelerated
    -- Window will show at upper left corner.
    GLFW.setWindowPosition 0 0
    -- Set the window title.
    GLFW.setWindowTitle "Arborgeddon"

    let shutdown = do
        GLFW.closeWindow
        GLFW.terminate
        game <- readIORef gameRef
        _    <- update' acid $ SaveGameState game
        _    <- exitSuccess
        putStrLn $ show game
        return True

    -- Register our keyboard input function.
    GLFW.setKeyCallback $ keyPressed shutdown
    -- Register our mouse position input function.
    GLFW.setMousePositionCallback mouseMoved
    -- Register our mouse button input function.
    GLFW.setMouseButtonCallback mouseButtonChanged
    -- Register our window close function.
    GLFW.setWindowCloseCallback shutdown
    blend     $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

    return True

initShaders :: IO Bool
initShaders = do
    cwd <- getCurrentDirectory
    let shaderDir = cwd </> "data" </> "shaders"
    putStrLn $ "Looking for shaders in '" ++ shaderDir ++ "'"
    v   <- loadShader $ shaderDir </> "color.vert" :: IO VertexShader
    f   <- loadShader $ shaderDir </> "color.frag" :: IO FragmentShader
    [p] <- genObjectNames 1
    attachedShaders p $= ([v],[f])
    attribLocation p "position" $= AttribLocation 0
    attribLocation p "color"    $= AttribLocation 1
    printError

    let glGet = Graphics.Rendering.OpenGL.get
    linkProgram p
    linked <- glGet $ linkStatus p
    unless linked $ do
        programLog <- glGet $ programInfoLog p
        putStrLn programLog

    -- Use this program.
    currentProgram $= Just p
    validateProgram p
    printError

    return True

updateMatrixUniforms :: Matrix GLfloat -> Matrix GLfloat -> IO ()
updateMatrixUniforms projection modelview = do
    p <- getCurrentProgram
    -- Get uniform locations
    projectionLoc <- withCString "projectionMat" $ \ptr ->
        glGetUniformLocation (programID p) ptr
    modelviewLoc <- withCString "modelviewMat" $ \ptr ->
        glGetUniformLocation (programID p) ptr
    printError

    -- Clear matrix uniforms.
    withArray (concat projection) $ \ptr ->
        glUniformMatrix4fv projectionLoc 1 1 ptr
    withArray (concat modelview) $ \ptr ->
        glUniformMatrix4fv modelviewLoc 1 1 ptr
    printError


displayOptions :: GLFW.DisplayOptions
displayOptions = GLFW.defaultDisplayOptions { GLFW.displayOptions_width  = 800
                                            , GLFW.displayOptions_height = 600
                                            -- Set depth buffering and RGBA colors
                                            , GLFW.displayOptions_numRedBits   = 8
                                            , GLFW.displayOptions_numGreenBits = 8
                                            , GLFW.displayOptions_numBlueBits  = 8
                                            , GLFW.displayOptions_numAlphaBits = 8
                                            , GLFW.displayOptions_numDepthBits = 1
                                            --, GLFW.displayOptions_displayMode = GLFW.Fullscreen
                                            }

vertexData :: [GLfloat]
vertexData = [  0.0,  0.9, 0.0
             , -0.9, -0.9, 0.0
             ,  0.9, -0.9, 0.0
             ]

colorData :: [GLfloat]
colorData = [ 1.0, 0.0, 0.0, 1.0
            , 0.0, 1.0, 0.0, 1.0
            , 0.0, 0.0, 1.0, 1.0
            ]

keyPressed :: IO Bool -> GLFW.KeyCallback
keyPressed shutdown = callback
    where callback GLFW.KeyEsc True = void shutdown
          callback a b = putStrLn $ isPressedString a b

mouseButtonChanged :: GLFW.MouseButtonCallback
mouseButtonChanged a b = putStrLn $ isPressedString a b

mouseMoved :: GLFW.MousePositionCallback
mouseMoved x y = return ()--putStrLn $ show x ++ ", " ++ show y

isPressedString :: (Show a) => a -> Bool -> String
isPressedString button pressed = show button ++ " is " ++ (if pressed then "pressed" else "not pressed") ++ "."

