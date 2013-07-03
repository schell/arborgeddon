{-# LANGUAGE TemplateHaskell #-}
module App.App where

import App.Clock
import App.Input
import App.TypeClasses
import Control.Monad.State
import Control.Monad.Loops
import System.Exit                   ( exitSuccess )
import Control.Lens                 hiding ( transform )
import Graphics.Rendering.OpenGL    hiding ( Matrix )
import qualified Graphics.UI.GLFW   as GLFW

data App a = App { _userData   :: a
                 , _userInput  :: Input
                 , _clock      :: Clock
                 }
makeLenses ''App
makeLenses ''Input
makeLenses ''InputState

initializeApp :: a -> IO (App a)
initializeApp a = do
    True <- initGLFW
    time <- getTime

    -- When True this gives us a moment
    -- to attach an OGL profiler.
    when False $ do
        putStrLn "Waiting for get line..."
        _         <- getLine
        return ()


    -- Register our resize window function.
    GLFW.setWindowSizeCallback (\_ _ -> do
        clear [ColorBuffer, DepthBuffer]
        GLFW.swapBuffers)

    let clock' = execState (tickClock time) emptyClock
    return App{ _userData = a
              , _clock = clock'
              , _userInput = emptyInput
              }

startApp :: UserData a => App a -> IO ()
startApp app = do
    userData' <- start $ app ^. userData
    let app' = userData .~ userData' $ app
    iterateM_ stepApp app'

stepApp :: UserData a => App a -> IO (App a)
stepApp app = do
    winOpen <- GLFW.windowIsOpen
    if winOpen then do
        t      <- getTime
        input' <- getInput $ app ^. userInput

        let clock'     = execState (tickClock t) $ app ^. clock
            userData'  = input input' $ app ^. userData
            dt:_       = clock' ^. frames
            userData'' = step dt userData'
            app'       = flip execState app $ do
                clock      .= clock'
                userInput  .= input'
                userData   .= userData''

        userData''' <- render userData''
        GLFW.swapBuffers
        return $ userData .~ userData''' $ app'
      else do
        end $ app ^. userData
        void shutdown
        return app

initGLFW :: IO Bool
initGLFW = do
    putStrLn "Initializing the OpenGL window and context."
    True <- GLFW.initialize
    -- Initialize the window.
    True <- GLFW.openWindow displayOptions
    -- Window will show at upper left corner.
    GLFW.setWindowPosition 0 0
    -- Set the window title.
    GLFW.setWindowTitle "Arborgeddon"

    blend     $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
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

shutdown :: IO Bool
shutdown = do
    GLFW.closeWindow
    GLFW.terminate
    --app <- readIORef appRef
    --_    <- update' acid $ SaveApp $ savedAppFromApp app
    --print app
    exitSuccess

