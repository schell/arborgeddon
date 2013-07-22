{-# LANGUAGE TemplateHaskell #-}
module Game.Game where

import App.TypeClasses
import App.Clock
import App.Input
import Graphics
import Geometry
import Data.Monoid
import Control.Monad.State
import Graphics.Rendering.OpenGL  ( clear, ClearBuffer(..) )
import System.FilePath  ( (</>) )
import Control.Lens hiding ( transform )
import qualified Data.IntMap as IM

data Game = GameLoad { _rsrcDef :: ResourceDef }
          | Game { _clock     :: Clock
                 , _userInput :: Input
                 , _rsrc      :: ResourceStore
                 , _scene     :: Scene DisplayObject
                 }
          | GameOver
makeLenses ''Game

instance UserData Game where
    -- | Will turn a resource definition into a renderable game.
    start  = startGame
    input  = inputGame
    step   = stepGame
    render = renderGame
    end    = endGame


loadGame :: Game
loadGame = GameLoad resource
     where resource = ResourceDef pgrams texs vbos
           pgrams   = [fontShaderProgram,colorShaderProgram]
           texs     = [fontTexDef]
           vbos     = [triDef, fontDef]
           fontDef  = VboDef "font" [fontVertArr, fontUVArr]
           triDef   = VboDef "tri" [triVerts, colorVerts]
           fontTexDef   = TexDef "font" 0 (texDir </> "text.png") runTexParams
           fontPs  = concat $ replicate 94 [0,0,0
                                           ,1,0,0
                                           ,1,1,0
                                           ,0,1,0
                                           ]
           fontUVs = concat [ [x*w,y*h
                              ,x*w+w,y*h
                              ,x*w+w,y*h+h
                              ,x*w,y*h+h] | y <- [0..5], x <- [0..31], w <- [8/256], h <- [8/256] ]
           triVerts   = AttributeData (Attribute Vec3 "position") triangle3d
           colorVerts = AttributeData (Attribute Vec4 "color") [ 1, 0, 0, 1
                                                               , 0, 1, 0, 1
                                                               , 0, 0, 1, 1
                                                               ]
           fontVertArr = AttributeData (Attribute Vec3 "position") fontPs
           fontUVArr   = AttributeData (Attribute Vec2 "uv") fontUVs


dataDir :: FilePath
dataDir = "/Users/schell/Code/arborgeddon/data/"

shaderDir :: FilePath
shaderDir = dataDir </> "shaders"

texDir :: FilePath
texDir = dataDir </> "textures"


fontShaderProgram :: ShaderProgram
fontShaderProgram =
    ShaderProgram { _shaderName = "font"
                  , _shaderSrc  = (shaderDir </> "font.vert",shaderDir </> "font.frag")
                  , _attributes = [ Attribute Vec3 "position"
                                  , Attribute Vec2 "uv"
                                  ]
                  , _uniforms   = [ Uniform Vec4 "color"
                                  , Uniform Mat4 "projection"
                                  , Uniform Mat4 "modelview"
                                  , Uniform Sam2D "sampler"
                                  ]
                  , _program    = Nothing
                  }


colorShaderProgram :: ShaderProgram
colorShaderProgram =
    ShaderProgram { _shaderName = "color"
                  , _shaderSrc  = (shaderDir </> "color.vert",shaderDir </> "color.frag")
                  , _attributes = [ Attribute Vec3 "position"
                                  , Attribute Vec2 "color"
                                  ]
                  , _uniforms   = [ Uniform Mat4 "projection"
                                  , Uniform Mat4 "modelview"
                                  ]
                  , _program    = Nothing
                  }

startGame :: Game -> IO Game
startGame (GameLoad rez) = do
        putStrLn "Loading resource def."
        mRStore <- loadResourceDef rez
        return $ case mRStore of
            Just rs -> Game { _clock     = emptyClock
                            , _userInput = emptyInput
                            , _scene     = makeGameScene
                            , _rsrc      = rs
                            }
            Nothing -> GameOver

startGame _ = error "Could not start game."


inputGame :: Input -> Game -> Game
inputGame _ g@(GameLoad _) = g

inputGame _ GameOver = GameOver

inputGame i@(Input _ es) g@(Game _ _ _ sg)  = flip execState g $ do
    userInput .= i
    scene .= foldl (flip inputEvent) sg es


inputEvent :: InputEvent -> Scene a -> Scene a
inputEvent (WindowSizeChangedTo (w,h)) (Scene _ _ ns ts) = Scene w h ns ts
inputEvent _ sg = sg


stepGame :: Clock -> Game -> Game
stepGame c (Game _ i sn rez) = Game c i sn rez
stepGame _  g = g


renderGame :: Game -> IO Game
renderGame g@(Game _ i rez sn) = do
    clear [ColorBuffer, DepthBuffer]
    renderEvents $ i ^. events
    renderScene rez sn
    return g
renderGame g = return g


renderEvents :: [InputEvent] -> IO ()
renderEvents = mapM_ renderEvent

renderEvent :: InputEvent -> IO ()
renderEvent = print


endGame :: t -> IO ()
endGame _ = putStrLn "Done."

makeGameScene :: Scene DisplayObject
makeGameScene = root
    where root   = Scene 0 0 (tris ++ [text]) containers
          containers = mempty 
          text   = Node [] (TextString "Booyah!") $ scale 16 16 1 mempty
          tris   = map tri [0..1000] 
          tri i  = Node [] ColoredTri $ scale 16 16 1 $ translate i i 0 mempty 

