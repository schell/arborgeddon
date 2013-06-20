module Game.Events (
    getInputEvents
) where

import Game.Types

import Graphics.UI.GLFW
import Control.Monad

-- | Returns the current input state and any input events that occurred
-- between the current and previous states.
getInputEvents :: InputState                    -- ^ The previous input state.
               -> IO (InputState, [InputEvent]) -- ^ The new input state and any input events.
getInputEvents input = do
    (keys, buttons, position) <- getCurrentInput
    let events      = keyEvents++buttonEvents++moveEvents
        keyEvents   = keyEventsBetween (_keysPressed input) keys
        buttonEvents= buttonEventsBetween (_mouseButtonsPressed input) buttons
        moveEvents  = [ MouseMovedTo position | position /= _mousePosition input ]
    return (InputState keys position buttons, events)

keyEventsBetween :: [Key] -> [Key] -> [InputEvent]
keyEventsBetween = makeEventsBetween (KeyButtonDown, KeyButtonUp)

buttonEventsBetween :: [MouseButton] -> [MouseButton] -> [InputEvent]
buttonEventsBetween = makeEventsBetween (MouseButtonDown, MouseButtonUp)

makeEventsBetween :: Eq a1 => (a1 -> a, a1 -> a) -> [a1] -> [a1] -> [a]
makeEventsBetween (addC, removeC) from to = fmap addC added ++ fmap removeC removed
    where added   = [ x | x <- from++to, x `notElem` from, x `elem` to ]
          removed = [ x | x <- from++to, x `notElem` to, x `elem` from ]

getCurrentInput :: IO ([Key], [MouseButton], (Int, Int))
getCurrentInput = do
    buttons  <- buttonsPressed
    position <- getMousePosition
    keys     <- keysPressed
    return (keys, buttons, position)

buttonsPressed :: IO [MouseButton]
buttonsPressed = listOfPolledInputPressed mouseButtonIsPressed mouseButtonsUsed

keysPressed :: IO [Key]
keysPressed = listOfPolledInputPressed keyIsPressed keysUsed

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
