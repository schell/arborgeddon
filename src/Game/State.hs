{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.State where

import Game.Types
import Geometry.Types

import Data.Acid
import Data.SafeCopy
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Control.Lens
import Foreign.C.Types

import Data.Monoid      ( mempty )

{- Lenses -}
makeLenses ''GameState
makeLenses ''DisplayElement
makeLenses ''InputState
makeLenses ''SavedGameState

{- Getters, Mutators -}
rotation :: Lens' (DisplayElement a) (Rotation3d a)
rotation = transform._1

translation :: Lens' (DisplayElement a) (Translation3d a)
translation = transform._3

scale :: Lens' (DisplayElement a) (Scale3d a)
scale = transform._2

{- Sane Defaults, Saving and Unsaving -}
$(deriveSafeCopy 0 'base ''SavedGameState)

defaultInput :: InputState
defaultInput = InputState [] (0,0) []

defaultDisplayElement :: Num a => DisplayElement a
defaultDisplayElement = DisplayElement [] mempty (\_ -> return ())

defaultGame :: Num a => GameState a
defaultGame = GameState defaultDisplayElement 0 0 defaultInput []

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

