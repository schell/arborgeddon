module Main where

import App
import Graphics.Resource
import Geometry.Matrix
import Control.Lens
import Data.Maybe
import Graphics.Rendering.OpenGL
import System.FilePath  ( (</>) )
import qualified Data.Map as M

main :: IO ()
main = do
    app     <- initializeApp loadGame
    startApp app

data Game = GameLoad ResourceDef | Game Renderable | GameOver

instance UserData Game where
    -- | Will turn a resource definition into a renderable game.
    start  = startGame
    input  = inputGame
    step   = stepGame
    render = renderGame
    end    = endGame

loadGame :: Game
loadGame = GameLoad resource
     where resource = ResourceDef programs texs vbos
           programs = [texShaderDef]
           texs     = [fontTexDef]
           vbos     = [fontDef]
           dataDir  = "/Users/schell/Code/arborgeddon/data/"
           shaderDir= dataDir </> "shaders"
           texDir   = dataDir </> "textures"
           fontDef  = VboDef "text" [fontVertArr, fontUVArr]
           fontTexDef   = TexDef "text" 0 (texDir </> "text.png")
           texShaderDef = ProgramDef "texShader" (shaderDir </> "tex.vert") (shaderDir </> "tex.frag") ["position","uv"]
           fontPs  = [0,0,0
                     ,8,0,0
                     ,0,8,0
                     ,8,8,0
                     ]
           fontUVs = [0,0
                     ,8/256,0
                     ,0,8/256
                     ,8/256,8/256
                     ]
           fontVertArr = AttribArrayDef 3 fontPs
           fontUVArr   = AttribArrayDef 2 fontUVs


startGame :: Game -> IO Game
startGame (GameLoad rez) = do
        putStrLn $ "Loading resources: " ++ show rez
        mRStore <- loadResourceDef rez
        return $ case mRStore of
            Just rs -> Game $ makeGameRenderable rs
            Nothing -> GameOver
startGame _ = error "Could not start game."

inputGame :: Input -> Game -> Game
inputGame _ = id

stepGame :: t -> a -> a
stepGame _ = id

renderGame :: Game -> IO Game
renderGame (Game r) = do
    r ^. runRender $ r
    return $ Game r
renderGame game = return game

endGame :: t -> IO ()
endGame _ = putStrLn "Done."

makeGameRenderable :: ResourceStore -> Renderable
makeGameRenderable rs =
    Renderable ps bs ts us (\_ -> return ())
        where ps = maybe [] (\p -> [_program p]) (M.lookup "texShader" $ rs ^. programMap)
              bs = maybeToList (M.lookup "text" $ rs ^. vboMap)
              ts = maybeToList (M.lookup "text" $ rs ^. textureMap)
              us = [concat $ identityN 4, concat $ identityN 4]

renderText :: Game -> IO Game
renderText (Game (Renderable (p:ps) (v:vs) (t:ts) us _)) = undefined


