module App.TypeClasses where

import App.Input

type DeltaTime = Double

class UserData a where
    start  :: a -> IO a
    input  :: Input -> a -> a
    step   :: DeltaTime -> a -> a
    render :: a -> IO a
    end    :: a -> IO ()

