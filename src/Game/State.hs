{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.State where

import Game.Types

import Data.Acid
import Data.SafeCopy
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Foreign.C.Types

import Data.Monoid      ( mempty )


{- Sane Defaults, Saving and Unsaving -}
$(deriveSafeCopy 0 'base ''SavedGameState)

defaultInput :: InputState
defaultInput = InputState [] (0,0) []

defaultDisplayElement :: Num a => DisplayElement a
defaultDisplayElement = DisplayElement [] mempty

defaultSprite :: Sprite2d String
defaultSprite = Sprite2d "defaultSprite" 1.0 0.0 0 []  

defaultGame :: Num a => GameState a
defaultGame = GameState defaultSprite 0 0 defaultInput [] (undefined, undefined)

gameFromSavedGame :: Num a => SavedGameState a -> GameState a
gameFromSavedGame _ = defaultGame

savedGameFromGame :: Num a => GameState a -> SavedGameState a
savedGameFromGame _ = defaultSavedGame

defaultSavedGame :: Num a => SavedGameState a
defaultSavedGame = savedGameFromGame defaultGame

unsaveGame :: Query (SavedGameState a) (SavedGameState a)
unsaveGame = ask

saveGame :: SavedGameState a -> Update (SavedGameState a) ()
saveGame = put

$(makeAcidic ''SavedGameState ['saveGame, 'unsaveGame])

instance SafeCopy CFloat where
    putCopy (CFloat f) = contain $ safePut f
    getCopy = contain $ CFloat <$> safeGet

