module App.TypeClasses where

import App.Input
import App.Clock

class UserData a where
    start  :: a -> IO a
    input  :: Input -> a -> a
    step   :: Clock -> a -> a
    render :: a -> IO a
    end    :: a -> IO ()

