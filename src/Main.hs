module Main where

import Graphics
import Geometry
import Game

import Data.IORef
import Data.Acid
import Control.Monad.State
import Debug.Trace
import Graphics.Rendering.OpenGL.GL.Shaders.Program

import Data.Acid.Local               ( createCheckpointAndClose )
import Data.Acid.Advanced            ( query', update' )
import Control.Exception             ( bracket )
import System.Exit                   ( exitSuccess )
import System.Directory              ( getCurrentDirectory )
import System.FilePath               ( (</>) )
import Foreign.Ptr                   ( Ptr )
import Foreign.Marshal.Array         ( withArray )
import Foreign.C.String              ( withCString )
import Graphics.Rendering.OpenGL.Raw ( glGetUniformLocation, glUniformMatrix4fv, glUniform1i )

import Control.Lens                 hiding ( transform )
import Graphics.Rendering.OpenGL    hiding ( Matrix )
import qualified Graphics.UI.GLFW   as GLFW

type DisplayState = (TextureObject, InterleavedVbo)
type Game = GameState GLfloat
type SavedGame = SavedGameState GLfloat

main :: IO ()
main = do
    putStrLn "Starting Arborgeddon..."
    bracket (openLocalState defaultSavedGame)
            createCheckpointAndClose
            runGame

runGame :: AcidState SavedGame -> IO ()
runGame acid = do
    savedGame <- query' acid UnsaveGame
    gameRef   <- newIORef $ gameFromSavedGame savedGame
    True      <- initGLFW acid gameRef

    -- putStrLn "Waiting for get line..."
    -- _         <- getLine

    True      <- initShaders

    -- Declare our texture settings.
    texture Texture2D $= Enabled
    -- Load our textures or die.
    Just t <- loadTexture "/Users/schell/Code/arborgeddon/data/textures/test.png"
    textureFilter   Texture2D   $= ((Nearest, Nothing), Nearest)
    textureWrapMode Texture2D S $= (Repeated, Repeat)
    textureWrapMode Texture2D T $= (Repeated, Repeat)


    -- Vertex data things.
    let vertexData = square :: [GLfloat]
        texData    = [ 0.0, 0.0
                     , 0.0, 1.0
                     , 1.0, 1.0
                     , 1.0, 1.0
                     , 0.0, 0.0
                     , 1.0, 0.0
                     ]
        colorData  = concat $ replicate 2 colorTri
        colorTri   = [ 1.0, 0.0, 0.0, 1.0
                     , 0.0, 1.0, 0.0, 1.0
                     , 0.0, 0.0, 1.0, 1.0
                     ]
    ivbo <- interleavedVbo [vertexData, texData] [3,2] [AttribLocation 0, AttribLocation 1]
    putStrLn $ "Got ivbo "++show ivbo
    let rsrc = (t, ivbo)

    -- Register our resize window function.
    GLFW.setWindowSizeCallback (\w h -> do
        -- This essentially pauses the game but continues to draw the
        -- scene.
        viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
        game <- readIORef gameRef
        drawScene game rsrc)

    forever $ do
        stepAndDrawGame gameRef rsrc
        -- Comment this out for releases.
        do
            game <- readIORef gameRef
            when (all (`elem` game^.input^.keysPressed) [GLFW.KeyLeftCtrl, GLFW.CharKey 'T']) $ print game
            unless (null $ game^.events) $ print $ game^.events
            when (KeyButtonDown GLFW.KeyEsc `elem` game^.events) $ void $ shutdown acid gameRef

stepAndDrawGame :: IORef Game -> DisplayState -> IO ()
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

drawScene :: Game -> DisplayState -> IO ()
drawScene game (tex, ivbo) = do
    -- Clear the screen and the depth buffer.
    clear [ColorBuffer, DepthBuffer]
    -- Update the matrix uniforms.
    let t  = game^.scene.transform
        mv = applyTransformation t $ identityN 4
        p  = identityN 4
    -- Texture
    texture Texture2D $= Enabled
    activeTexture     $= TextureUnit 0
    textureBinding Texture2D $= Just tex
    printError

    updateMatrixUniforms p mv
    -- Geometry
    bindInterleavedVbo ivbo
    drawArrays Triangles 0 6
    printError
    GLFW.swapBuffers

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

shutdown :: AcidState SavedGame -> IORef Game -> IO Bool
shutdown acid gameRef = trace "shutting down" $ do
    GLFW.closeWindow
    GLFW.terminate
    --game <- readIORef gameRef
    --_    <- update' acid $ SaveGame $ savedGameFromGame game
    --print game
    exitSuccess

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

updateUniforms :: [String]                                 -- ^ A list of uniform names.
               -> [GLint -> Ptr GLfloat -> IO ()] -- ^ A list of update functions.
               -> [[GLfloat]]                              -- ^ A list of uniform arrays.
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

