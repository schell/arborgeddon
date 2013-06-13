module Main where

import Graphics
import Geometry
import Game
import Game.Clock

import Foreign.Ptr
import Data.Acid
import System.CPUTime

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
    True <- initGLFW acid
    True <- initShaders

    -- Vertex data things.
    ivbo <- interleavedVbo [vertexData, colorData] [3,4] [AttribLocation 0, AttribLocation 1]

    -- Register our scene drawing function.
    --GLFW.setWindowRefreshCallback windowRefresh
    -- Register our resize window function.
    GLFW.setWindowSizeCallback (\w h -> do
        game    <- query' acid GetGameState
        let game' = game{gsWindow = (gsWindow game){gwSize = (w, h)}}
        _ <- update' acid $ SaveGameState game'
        viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
        stepGame acid ivbo)

    forever $ stepGame acid ivbo


stepGame :: AcidState GameState -> InterleavedVbo -> IO ()
stepGame acid ivbo = do
    game    <- query' acid GetGameState
    newGame <- updateGameState game
    _       <- update' acid $ SaveGameState newGame

    drawScene newGame ivbo

updateGameState :: GameState -> IO GameState
updateGameState gamePrev = do
    newTime <- getTime
    -- Update the time steps.
    let game = gamePrev { gsTimePrev = gsTimeNow gamePrev, gsTimeNow = newTime }
    return $ step game


drawScene :: GameState -> DisplayState -> IO ()
drawScene game ivbo = do
    -- Clear the screen and the depth buffer.
    clear [ColorBuffer, DepthBuffer]
    -- Update the matrix uniforms.
    let t = gsTransform game
    updateMatrixUniforms (identityN 4) (applyTransformation t $ identityN 4)
    bindInterleavedVbo ivbo
    drawArrays Triangles 0 3
    printError
    GLFW.swapBuffers

initGLFW :: AcidState GameState -> IO Bool
initGLFW acid = do
    putStrLn "Initializing the OpenGL window and context."

    True <- GLFW.initialize
    -- Initialize the window.
    game <- query' acid GetGameState
    let displayOps = displayOptions{ GLFW.displayOptions_width  = fst $ gwSize $ gsWindow game
                                   , GLFW.displayOptions_height = snd $ gwSize $ gsWindow game
                                   }
    True <- GLFW.openWindow displayOps
    -- Make sure the window is gpu'able.
    True <- GLFW.windowIsHardwareAccelerated
    -- Window will show at upper left corner.
    GLFW.setWindowPosition 0 0
    -- Set the window title.
    GLFW.setWindowTitle "Arborgeddon"
    -- Register our keyboard input function.
    GLFW.setKeyCallback keyPressed
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

    linkProgram p
    linked <- get $ linkStatus p
    unless linked $ do
        programLog <- get $ programInfoLog p
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


keyPressed :: GLFW.KeyCallback
keyPressed GLFW.KeyEsc True = void shutdown
keyPressed a b = putStrLn $ isPressedString a b

mouseButtonChanged :: GLFW.MouseButtonCallback
mouseButtonChanged a b = putStrLn $ isPressedString a b

mouseMoved :: GLFW.MousePositionCallback
mouseMoved x y = putStrLn $ show x ++ ", " ++ show y

isPressedString :: (Show a) => a -> Bool -> String
isPressedString button pressed = show button ++ " is " ++ (if pressed then "pressed" else "not pressed") ++ "."

shutdown :: GLFW.WindowCloseCallback
shutdown = do
    GLFW.closeWindow
    GLFW.terminate
    _ <- exitSuccess
    return True

