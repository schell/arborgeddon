module Main where

import qualified Graphics.UI.GLFW   as GLFW
import Graphics.Rendering.OpenGL

import Arbor.Graphics.Shaders
import Arbor.Graphics.Util
import Arbor.Graphics.Vbo

import Control.Monad         ( void, when, unless, forever )
import System.Exit           ( exitSuccess )
import System.Directory      ( getCurrentDirectory )
import System.FilePath       ( (</>) )
import Foreign.Marshal.Array ( withArray )
import Foreign.Ptr           ( nullPtr )
import Foreign.Storable      ( sizeOf )

data Shaders = Shaders { vertexShader   :: VertexShader
                       , fragmentShader :: FragmentShader
                       , program        :: Program
                       , fadeFactorU    :: UniformLocation
                       , texturesU      :: [UniformLocation]
                       , positionA      :: AttribLocation }

main :: IO ()
main = do
    putStrLn "Starting Arborgeddon..."

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

    -- Shader stuff.
    cwd <- getCurrentDirectory
    let shaderDir = cwd </> "data" </> "shaders"
    putStrLn $ "Looking for shaders in '" ++ shaderDir ++ "'"
    v   <- loadShader $ shaderDir </> "test.vert" :: IO VertexShader
    f   <- loadShader $ shaderDir </> "test.frag" :: IO FragmentShader
    [p] <- genObjectNames 1
    attachedShaders p $= ([v],[f])
    attribLocation p "position" $= AttribLocation 0
    linkProgram p
    linked <- get $ linkStatus p
    unless linked $ do
        programLog <- get $ programInfoLog p
        putStrLn programLog

    -- Use this program.
    currentProgram $= Just p
    validateProgram p

    -- Vertex data things.
    vbo <- createVbo vertexData

    -- Register our scene drawing function.
    GLFW.setWindowRefreshCallback $ drawScene vbo
    -- Register our resize window function.
    GLFW.setWindowSizeCallback (\_ _ ->
       drawScene vbo)

    forever $ drawScene vbo

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
             ,  0.9, -0.9, 0.0 ]

-- bindVbo :: GLint -> GLuint -> BufferObject -> IO ()
-- bindVbo size loc vb = do
--     vertexAttribArray (AttribLocation loc) $= Enabled
--     bindBuffer ArrayBuffer $= Just vb
--     vertexAttribPointer (AttribLocation loc) $= (ToFloat, VertexArrayDescriptor size Float 0 nullPtr)

drawScene :: BufferObject -> IO ()
drawScene vbo = do
    -- Clear the screen and the depth buffer.
    clear [ColorBuffer, DepthBuffer]
    bindVbo 3 0 vbo
    drawArrays Triangles 0 3
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
