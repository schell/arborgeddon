module Main where

import App
import Control.Lens
import Control.Applicative
import Graphics.Resource
import Graphics.Rendering.OpenGL
import System.FilePath  ( (</>) )

main :: IO ()
main = do
    app     <- initializeApp $ GameLoad resource
    startApp app
        where resource = ResourceDef programs texs vbos
              programs = [texShaderDef]
              texs     = [fontTexDef]
              vbos     = [fontDef]
              dataDir  = "/Users/schell/Code/arborgeddon/data/"
              shaderDir= dataDir </> "shaders"
              attLocs  = AttribLocation . fromIntegral <$> [0,1]
              texShaderDef = ProgramDef (shaderDir </> "tex.vert") (shaderDir </> "tex.frag") ["position","uv"]
              texDir = dataDir </> "textures"
              fontTexDef = TexDef 0 (texDir </> "font.png")
              fontDef = VboDef "font" [fontVertArr, fontUVArr]
              fontPs = [0,0,0
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
              fontUVArr = AttribArrayDef 2 fontUVs


data Game = GameLoad ResourceDef | Game { _rsrc :: Renderable } | GameOver

instance UserData Game where
    -- | Will turn a resource definition into a renderable game.
    start  = startGame
    input  = inputGame
    step   = stepGame
    render = renderGame
    end    = endGame

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

renderGame :: b -> IO b
renderGame = return . id

endGame :: t -> IO ()
endGame _ = putStrLn "Done."

makeGameRenderable :: t -> Renderable
makeGameRenderable rs =
    Renderable [] [] [] [] render'
        where render' r = putStrLn "Rendered."
