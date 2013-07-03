module App.Input (
    getInput,
    emptyInput,
    Input(..),
    InputState(..),
    InputEvent(..)
) where

import Graphics.UI.GLFW
import Control.Monad

{- Defining -}
data Input = Input { _state      :: InputState
                   , _events     :: [InputEvent]
                   }

emptyInput :: Input
emptyInput = Input { _events = []
                   , _state = emptyInputState
                   }

data InputState = InputState { _keysPressed         :: [Key]
                             , _mousePosition       :: (Int, Int)
                             , _mouseButtonsPressed :: [MouseButton]
                             , _windowSize          :: (Int, Int)
                             } deriving (Show, Eq )

emptyInputState :: InputState
emptyInputState = InputState { _keysPressed = []
                             , _mousePosition = (0,0)
                             , _mouseButtonsPressed = []
                             , _windowSize = (0,0)
                             }

{- Events -}
data InputEvent = KeyButtonDown Key
                | KeyButtonUp Key
                | MouseButtonDown MouseButton
                | MouseButtonUp MouseButton
                | MouseMovedTo (Int, Int)
                | WindowSizeChangedTo (Int, Int)
                deriving (Show, Eq)

-- | Returns the current input state and any input events that occurred
-- between the current and previous states.
getInput :: Input    -- ^ The previous input state.
         -> IO Input -- ^ The new input state and any input events.
getInput (Input s _) = do
    (keys, buttons, position, (w, h)) <- getCurrentInput
    let events'     = keyEvents++buttonEvents++moveEvents++winEvents
        keyEvents   = getKeyEventsBetween (_keysPressed s) keys
        buttonEvents= getButtonEventsBetween (_mouseButtonsPressed s) buttons
        moveEvents  = [ MouseMovedTo position | position /= _mousePosition s ]
        winEvents   = if winSize == _windowSize s then [] else [WindowSizeChangedTo winSize]
        winSize     = (fromIntegral w, fromIntegral h)
    return $ Input (InputState keys position buttons winSize) events'

getKeyEventsBetween :: [Key] -> [Key] -> [InputEvent]
getKeyEventsBetween = makeEventsBetween (KeyButtonDown, KeyButtonUp)

getButtonEventsBetween :: [MouseButton] -> [MouseButton] -> [InputEvent]
getButtonEventsBetween = makeEventsBetween (MouseButtonDown, MouseButtonUp)

makeEventsBetween :: Eq a1 => (a1 -> a, a1 -> a) -> [a1] -> [a1] -> [a]
makeEventsBetween (addC, removeC) from to = fmap addC added ++ fmap removeC removed
    where added   = [ x | x <- from++to, x `notElem` from, x `elem` to ]
          removed = [ x | x <- from++to, x `notElem` to, x `elem` from ]

getCurrentInput :: IO ([Key], [MouseButton], (Int, Int), (Int, Int))
getCurrentInput = do
    buttons  <- getButtonsPressed
    position <- getMousePosition
    keys     <- getKeysPressed
    winSize  <- getWindowDimensions
    return (keys, buttons, position, winSize)

getButtonsPressed :: IO [MouseButton]
getButtonsPressed = listOfPolledInputPressed mouseButtonIsPressed mouseButtonsUsed

getKeysPressed :: IO [Key]
getKeysPressed = listOfPolledInputPressed keyIsPressed keysUsed

listOfPolledInputPressed :: Show a => (a -> IO Bool) -> [a] -> IO [a]
listOfPolledInputPressed fPoll = foldM (\acc a -> do
    isPressed <- fPoll a
    return $ if isPressed
             then a:acc
             else acc) []

keysUsed :: [Key]
keysUsed = specialKeys ++ map (CharKey . toEnum) [0..127]

specialKeys :: [Key]
specialKeys = [ KeyUnknown
              , KeySpace
              , KeySpecial
              , KeyEsc
              , KeyF1
              , KeyF2
              , KeyF3
              , KeyF4
              , KeyF5
              , KeyF6
              , KeyF7
              , KeyF8
              , KeyF9
              , KeyF10
              , KeyF11
              , KeyF12
              , KeyF13
              , KeyF14
              , KeyF15
              , KeyF16
              , KeyF17
              , KeyF18
              , KeyF19
              , KeyF20
              , KeyF21
              , KeyF22
              , KeyF23
              , KeyF24
              , KeyF25
              , KeyUp
              , KeyDown
              , KeyLeft
              , KeyRight
              , KeyLeftShift
              , KeyRightShift
              , KeyLeftCtrl
              , KeyRightCtrl
              , KeyLeftAlt
              , KeyRightAlt
              , KeyTab
              , KeyEnter
              , KeyBackspace
              , KeyInsert
              , KeyDel
              , KeyPageup
              , KeyPagedown
              , KeyHome
              , KeyEnd
              , KeyPad0
              , KeyPad1
              , KeyPad2
              , KeyPad3
              , KeyPad4
              , KeyPad5
              , KeyPad6
              , KeyPad7
              , KeyPad8
              , KeyPad9
              , KeyPadDivide
              , KeyPadMultiply
              , KeyPadSubtract
              , KeyPadAdd
              , KeyPadDecimal
              , KeyPadEqual
              , KeyPadEnter
              ]

mouseButtonsUsed :: [MouseButton]
mouseButtonsUsed = [ MouseButton0 ]

