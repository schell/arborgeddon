{-# LANGUAGE RecordWildCards #-}
module Main where

import Graphics.Shaders
import Graphics.Util
import Graphics.Vbo
import Geometry

import Control.Concurrent.MVar
import Graphics.Rendering.OpenGL.Raw ( glGetUniformLocation )

import Control.Monad                 ( void, unless, forever )
import System.Exit                   ( exitSuccess )
import System.Directory              ( getCurrentDirectory )
import System.FilePath               ( (</>) )
import Foreign.Marshal.Array         ( withArray )
import Foreign.C.String              ( withCString )
import Foreign.Ptr                   ( nullPtr )
import Foreign.Storable              ( sizeOf )
import Graphics.Rendering.OpenGL.Raw ( glUniformMatrix4fv )

import Graphics.Rendering.OpenGL hiding ( Matrix )
import Graphics.Rendering.OpenGL.GL.Shaders.Program
import qualified Graphics.UI.GLFW   as GLFW

data AppState = App { appWindowSize :: (Int, Int) }

main :: IO ()
main = do
    putStrLn "Starting Arborgeddon..."

    appState <- newEmptyMVar

    True <- GLFW.initialize
    -- Get a 640 x 480 window.
    -- Initialize the window.
    True <- GLFW.openWindow displayOptions
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

    -- Shader stuff.
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

    -- Get uniform locations
    projectionLoc <- withCString "projectionMat" $ \ptr ->
        glGetUniformLocation (programID p) ptr
    modelviewLoc <- withCString "modelviewMat" $ \ptr ->
        glGetUniformLocation (programID p) ptr
    printError

    activeUs <- get $ activeUniforms p
    putStrLn $ "Active uniforms: "++show activeUs
    putStrLn $ "Uniform locations: "++show [projectionLoc,modelviewLoc]

    let projectionMat = [[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]] :: Matrix GLfloat
        modelviewMat  = scale3d (1/2) (1/2) 1 $ identity projectionMat
        matrixSize    = fromIntegral $ 16 * sizeOf (undefined :: GLfloat)

    putStrLn "Updating uniforms."
    withArray (concat projectionMat) $ \ptr ->
        glUniformMatrix4fv projectionLoc 1 1 ptr
    withArray (concat modelviewMat) $ \ptr ->
        glUniformMatrix4fv modelviewLoc 1 1 ptr
    printError

    -- Enable the attribs.

    -- Vertex data things.
    ivbo <- interleavedVbo [vertexData, colorData] [3,4] [AttribLocation 0, AttribLocation 1]

    -- Register our scene drawing function.
    GLFW.setWindowRefreshCallback $ drawScene ivbo --cbo
    -- Register our resize window function.
    GLFW.setWindowSizeCallback (\w h -> do
        viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
        _ <- tryTakeMVar appState
        putMVar appState App{ appWindowSize = (w, h) }
        drawScene ivbo {-cbo-})

    forever $ drawScene ivbo



displayOptions :: GLFW.DisplayOptions
displayOptions = GLFW.defaultDisplayOptions { GLFW.displayOptions_width  = 800
                                            , GLFW.displayOptions_height = 600
                                            -- Set depth buffering and RGBA colors
                                            , GLFW.displayOptions_numRedBits   = 8
                                            , GLFW.displayOptions_numGreenBits = 8
                                            , GLFW.displayOptions_numBlueBits  = 8
                                            , GLFW.displayOptions_numAlphaBits = 8
                                            , GLFW.displayOptions_numDepthBits = 1
                                            -- , GLFW.displayOptions_displayMode = GLFW.Fullscreen
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

-- bindVbo :: GLint -> GLuint -> BufferObject -> IO ()
-- bindVbo size loc vb = do
--     vertexAttribArray (AttribLocation loc) $= Enabled
--     bindBuffer ArrayBuffer $= Just vb
--     vertexAttribPointer (AttribLocation loc) $= (ToFloat, VertexArrayDescriptor size Float 0 nullPtr)

drawScene :: InterleavedVbo -> IO ()
drawScene ivbo = do
    -- Clear the screen and the depth buffer.
    clear [ColorBuffer, DepthBuffer]
    bindInterleavedVbo ivbo
    drawArrays Triangles 0 3
    printError
    GLFW.swapBuffers

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
    exitSuccess
    return True

