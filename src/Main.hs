module Main where

import Graphics
import Geometry
import Game

import Data.IORef
import Data.Acid
import Control.Monad.State
import Control.Lens hiding ( transform )

import Data.Acid.Local               ( createCheckpointAndClose )
import Data.Acid.Advanced            ( query', update' )
import Control.Exception             ( bracket )
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
    bracket (openLocalState defaultSavedGame)
            createCheckpointAndClose
            runGame

runGame :: AcidState SavedGameState -> IO ()
runGame acid = do
    savedGame <- query' acid UnsaveGame
    gameRef   <- newIORef $ gameFromSavedGame savedGame
    True      <- initGLFW acid gameRef
    True      <- initShaders

    -- Vertex data things.
    ivbo <- interleavedVbo [vertexData, colorData] [3,4] [AttribLocation 0, AttribLocation 1]

    -- Register our resize window function.
    GLFW.setWindowSizeCallback (\w h -> do
        -- This essentially pauses the game but continues to draw the
        -- scene.
        viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
        game <- readIORef gameRef
        drawScene game ivbo)

    forever $ do
        stepAndDrawGame gameRef ivbo
        -- Comment this out for releases.
        do
            game <- readIORef gameRef
            when (KeyButtonDown GLFW.KeyEsc `elem` game^.events) $ void $ shutdown acid gameRef
            unless (null $ game^.events) $ print $ game^.events
            when (all (`elem` game^.input^.keysPressed) [GLFW.KeyLeftCtrl, GLFW.CharKey 'T']) $ print game

stepAndDrawGame :: IORef GameState -> DisplayState -> IO ()
stepAndDrawGame gameRef ivbo = do
    -- Update viewport, poll and get our events.
    (w, h)  <- GLFW.getWindowDimensions
    viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))

    gamePrev       <- readIORef gameRef
    (input_,vents) <- getInputEvents $ gamePrev^.input
    newTime        <- getTime

    -- Tie into our pure code.
    let newGame = step game
        game    = flip execState gamePrev $ do
            t <- use timeNow
            timePrev .= t
            timeNow  .= newTime
            events   .= vents
            input    .= input_

    writeIORef gameRef newGame
    drawScene newGame ivbo

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

initGLFW :: AcidState SavedGameState -> IORef GameState -> IO Bool
initGLFW acid gameRef = do
    putStrLn "Initializing the OpenGL window and context."

    True <- GLFW.initialize
    -- Initialize the window.
    True <- GLFW.openWindow displayOptions
    -- Window will show at upper left corner.
    GLFW.setWindowPosition 0 0
    -- Set the window title.
    GLFW.setWindowTitle "Arborgeddon"

    -- Register our window close function.
    GLFW.setWindowCloseCallback $ shutdown acid gameRef
    blend     $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    return True

shutdown :: AcidState SavedGameState -> IORef GameState -> IO Bool
shutdown acid gameRef = do
    GLFW.closeWindow
    GLFW.terminate
    game <- readIORef gameRef
    _    <- update' acid $ SaveGame $ savedGameFromGame game
    _    <- exitSuccess
    print game
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

