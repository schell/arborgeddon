{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeSynonymInstances, FlexibleInstances #-}
module Game.Game where

import Game.Types
import Game.Event.Input
import Game.Clock
import Graphics

import Control.Lens              hiding ( transform )


data Game = Game { _scene       :: Scene
                 , _clock       :: Clock
                 , _windowSize  :: (Int, Int)
                 , _input       :: InputState
                 , _inputEvents :: [InputEvent]
                 }
makeLenses ''Game

renderGame :: Game -> IO ()
renderGame game = do
    let (wi,hi) = game^.windowSize
        (w,h)   = (fromIntegral wi, fromIntegral hi)
    renderScene (w,h) $ game^.scene

emptyGame :: Game
emptyGame = Game emptyScene emptyClock (0,0) emptyInput []

