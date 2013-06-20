{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.State where

import Game.Types
import Geometry.Types

import Data.Acid
import Data.SafeCopy
import Graphics.Rendering.OpenGL.Raw
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Control.Lens
import Foreign.C.Types

import Data.Monoid      ( mempty )

{- Lenses -}
makeLenses ''GameState
makeLenses ''InputState
makeLenses ''SavedGameState

{- Getters, Mutators -}
rotation :: Lens' GameState (Rotation3d GLfloat)
rotation = transform._1

translation :: Lens' GameState (Translation3d GLfloat)
translation = transform._3

scale :: Lens' GameState (Scale3d GLfloat)
scale = transform._2

{- Sane Defaults, Saving and Unsaving -}
$(deriveSafeCopy 0 'base ''SavedGameState)

defaultInput :: InputState
defaultInput = InputState [] (0,0) []

defaultGame :: GameState
defaultGame = GameState (mempty :: Transform3d GLfloat) 0 0 defaultInput []

gameFromSavedGame :: SavedGameState -> GameState
gameFromSavedGame sg = defaultGame{_transform=sg^.savedTransform}

savedGameFromGame :: GameState -> SavedGameState
savedGameFromGame g = SavedGameState{_savedTransform=g^.transform}

defaultSavedGame :: SavedGameState
defaultSavedGame = savedGameFromGame defaultGame

unsaveGame :: Query SavedGameState SavedGameState
unsaveGame = ask

saveGame :: SavedGameState -> Update SavedGameState ()
saveGame = put

$(makeAcidic ''SavedGameState ['saveGame, 'unsaveGame])

instance SafeCopy CFloat where
    putCopy (CFloat f) = contain $ safePut f
    getCopy = contain $ CFloat <$> safeGet

