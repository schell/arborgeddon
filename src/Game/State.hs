{-# LANGUAGE DeriveDataTypeable, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.State where

import Game.Game
import Game.Types
import Game.Sprites

import Data.Acid
import Data.SafeCopy
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Foreign.C.Types

import Data.Monoid      ( mempty )

{- Sane Defaults, Saving and Unsaving -}
-- $(deriveSafeCopy 0 'base ''SavedGame)

defaultInput :: InputState
defaultInput = InputState [] (0,0) []

defaultDisplayElement :: Num a => DisplayElement a
defaultDisplayElement = DisplayElement [] mempty

defaultSprite :: Sprite
defaultSprite = Sprite2d Nothing 1.0 0.0 0 [] ""

defaultGame :: Game
defaultGame = GameState (scooterSprite :: Sprite) 0 0 defaultInput [] Nothing

gameFromSavedGame :: SavedGame -> Game
gameFromSavedGame _ = defaultGame

savedGameFromGame :: Game -> SavedGame
savedGameFromGame _ = defaultSavedGame

defaultSavedGame :: SavedGame
defaultSavedGame = savedGameFromGame defaultGame

unsaveGame :: Query SavedGame SavedGame
unsaveGame = ask

saveGame :: SavedGame -> Update SavedGame ()
saveGame = put

-- $(makeAcidic ''SavedGameState ['saveGame, 'unsaveGame])

instance SafeCopy CFloat where
    putCopy (CFloat f) = contain $ safePut f
    getCopy = contain $ CFloat <$> safeGet

