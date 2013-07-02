module Main where

import Graphics
import Game

import Data.IORef
import Control.Monad.State
import Debug.Trace
import Graphics.Rendering.OpenGL.GL.Shaders.Program

import System.Exit                   ( exitSuccess )
import System.Directory              ( getCurrentDirectory )
import System.FilePath               ( (</>) )

import Control.Lens                 hiding ( transform )
import Graphics.Rendering.OpenGL    hiding ( Matrix )
import qualified Graphics.UI.GLFW   as GLFW

main :: IO ()
main = do
    time      <- getTime
    let clock' = execState (tickClock time) emptyClock
        game   = emptyGame{_clock = clock'}
    gameRef   <- newIORef game
    True      <- initGLFW gameRef
    -- When True this gives us a moment
    -- to attach an OGL profiler.
    when False $ do
        putStrLn "Waiting for get line..."
        _         <- getLine
        return ()

    -- Finish init'ing.
    True      <- initShaders
    writeIORef gameRef game

    -- Register our resize window function.
    GLFW.setWindowSizeCallback $ resizeWindow gameRef

    -- Render loop.
    forever $ stepAndRender gameRef

resizeWindow :: IORef Game -> GLFW.WindowSizeCallback
resizeWindow gameRef w h = do
    game    <- readIORef gameRef
    t       <- getTime
    let clock' = execState (tickClock t) $ game^.clock
        game' = flip execState game $ do
        clock      .= clock'
        windowSize .= (w,h)

    writeIORef gameRef game'
    renderGame game'

stepAndRender :: IORef Game -> IO ()
stepAndRender gameRef = do
    game <- readIORef gameRef
    t    <- getTime
    (input',inputEvents') <- getInputEvents $ game^.input
    -- Tie into our pure code.
    let game'' = step game'
        clock' = execState (tickClock t) $ game^.clock
        game'  = flip execState game $ do
            clock       .= clock'
            inputEvents .= inputEvents'
            input       .= input'

    writeIORef gameRef game''
    renderGame game''

initGLFW :: IORef Game -> IO Bool
initGLFW gameRef = do
    putStrLn "Initializing the OpenGL window and context."

    True <- GLFW.initialize
    -- Initialize the window.
    True <- GLFW.openWindow displayOptions
    -- Window will show at upper left corner.
    GLFW.setWindowPosition 0 0
    -- Set the window title.
    GLFW.setWindowTitle "Arborgeddon"

    -- Register our window close function.
    GLFW.setWindowCloseCallback $ shutdown gameRef

    blend     $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    return True

initShaders :: IO Bool
initShaders = do
    cwd <- getCurrentDirectory
    let shaderDir = cwd </> "data" </> "shaders"
    putStrLn $ "Looking for shaders in '" ++ shaderDir ++ "'"
    v   <- loadShader $ shaderDir </> "tex.vert" :: IO VertexShader
    f   <- loadShader $ shaderDir </> "tex.frag" :: IO FragmentShader
    [p] <- genObjectNames 1
    attachedShaders p $= ([v],[f])
    attribLocation p "position" $= AttribLocation 0
    attribLocation p "uv"       $= AttribLocation 1
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

shutdown :: IORef Game -> IO Bool
shutdown _ = trace "shutting down" $ do
    GLFW.closeWindow
    GLFW.terminate
    --game <- readIORef gameRef
    --_    <- update' acid $ SaveGame $ savedGameFromGame game
    --print game
    exitSuccess

