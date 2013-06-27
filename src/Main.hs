module Main where

import Graphics
import Game

import Data.IORef
import Data.Acid
import Control.Monad.State
import Debug.Trace
import Graphics.Rendering.OpenGL.GL.Shaders.Program

--import Data.Acid.Local               ( createCheckpointAndClose )
--import Data.Acid.Advanced            ( query', update' )
--import Control.Exception             ( bracket )
import System.Exit                   ( exitSuccess )
import System.Directory              ( getCurrentDirectory )
import System.FilePath               ( (</>) )

import Control.Lens                 hiding ( transform )
import Graphics.Rendering.OpenGL    hiding ( Matrix )
import qualified Graphics.UI.GLFW   as GLFW

main :: IO ()
main =  -- bracket (openLocalState defaultSavedGame)
    -- createCheckpointAndClose
    runGame undefined

runGame :: AcidState SavedGame -> IO ()
runGame acid = do
    -- savedGame <- query' acid UnsaveGame
    time      <- getTime
    let prevGame = execState (timeNow .= time) defaultGame
    gameRef   <- newIORef prevGame
    True      <- initGLFW acid gameRef
    -- When True this gives us a moment
    -- to attach an OGL profiler.
    when False $ do
        putStrLn "Waiting for get line..."
        _         <- getLine
        return ()

    -- Finish init'ing.
    True      <- initShaders
    initdGame <- initRsrcs Nothing prevGame
    writeIORef gameRef initdGame

    -- Register our resize window function.
    GLFW.setWindowSizeCallback (\w h -> do
        -- This essentially pauses the game but continues to draw the
        -- scene.
        viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
        game <- readIORef gameRef
        t    <- getTime
        let game' = flip execState game $ do
            concatTime t
            windowSize .= (w,h) 

        writeIORef gameRef game'
        void $ render game')

    -- Render loop.
    forever $ do
        stepAndRender gameRef
        -- Comment this out for releases.
        do
            game <- readIORef gameRef
            when (all (`elem` game^.input^.keysPressed) [GLFW.KeyLeftCtrl, GLFW.CharKey 'T']) $ print game
            unless (null $ game^.events) $ print $ game^.events

stepAndRender :: IORef Game -> IO ()
stepAndRender gameRef = do
    -- Update viewport, poll and get our events.
    (w, h)  <- GLFW.getWindowDimensions
    viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))

    gamePrev         <- readIORef gameRef
    (input',events') <- getInputEvents $ gamePrev^.input
    newTime          <- getTime

    -- Tie into our pure code.
    let newGame = step game
        game    = flip execState gamePrev $ do
            concatTime newTime
            events .= events'
            input  .= input'

    render newGame
    writeIORef gameRef newGame

initGLFW :: AcidState SavedGame -> IORef Game -> IO Bool
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

shutdown :: AcidState SavedGame -> IORef Game -> IO Bool
shutdown acid gameRef = trace "shutting down" $ do
    GLFW.closeWindow
    GLFW.terminate
    --game <- readIORef gameRef
    --_    <- update' acid $ SaveGame $ savedGameFromGame game
    --print game
    exitSuccess

