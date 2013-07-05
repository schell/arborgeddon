module Main where

import App
import Graphics
import Geometry
import Control.Lens
import Control.Monad
import Data.Maybe
import Graphics.Rendering.OpenGL.Raw
import System.FilePath  ( (</>) )
import qualified Data.Map as M
import Graphics.Rendering.OpenGL hiding ( Matrix )

main :: IO ()
main = do
    app     <- initializeApp loadGame
    startApp app

data Game = GameLoad ResourceDef | Game Clock Input Renderable | GameOver

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
           pgrams   = [colorShaderDef, texShaderDef]
           texs     = [fontTexDef]
           vbos     = [triDef, fontDef]
           dataDir  = "/Users/schell/Code/arborgeddon/data/"
           shaderDir= dataDir </> "shaders"
           texDir   = dataDir </> "textures"
           fontDef  = VboDef "text" [fontVertArr, fontUVArr]
           triDef   = VboDef "tri" [triVerts, colorVerts]
           fontTexDef   = TexDef "text" 0 (texDir </> "text.png") runTexParams
           texShaderDef = ProgramDef "texShader" (shaderDir </> "tex.vert") (shaderDir </> "tex.frag") ["position","uv"]
           colorShaderDef = ProgramDef "colorShader" (shaderDir </> "color.vert") (shaderDir </> "color.frag") ["position", "color"]
           fontPs  = concat $ replicate 94 [0,0,0
                                           ,1,0,0
                                           ,1,1,0
                                           ,0,1,0
                                           ]
           fontUVs = concat [ [x*w,y*h
                              ,x*w+w,y*h
                              ,x*w+w,y*h+h
                              ,x*w,y*h+h] | y <- [0..5], x <- [0..31], w <- [8/256], h <- [8/256] ]
           triVerts   = AttribArrayDef 3 triangle3d
           colorVerts = AttribArrayDef 4 [ 1, 0, 0, 1
                                         , 0, 1, 0, 1
                                         , 0, 0, 1, 1
                                         ]
           fontVertArr = AttribArrayDef 3 fontPs
           fontUVArr   = AttribArrayDef 2 fontUVs

runTexParams :: TexParamsRunner
runTexParams = TexParamsRunner $ do
    textureFilter   Texture2D   $= ((Nearest, Nothing), Nearest)
    textureWrapMode Texture2D S $= (Repeated, Clamp)
    textureWrapMode Texture2D T $= (Repeated, Clamp)

startGame :: Game -> IO Game
startGame (GameLoad rez) = do
        putStrLn $ "Loading resources: " ++ show rez
        mRStore <- loadResourceDef rez
        return $ case mRStore of
            Just rs -> Game emptyClock emptyInput $ makeGameRenderable rs
            Nothing -> GameOver
startGame _ = error "Could not start game."

inputGame :: Input -> Game -> Game
inputGame i (Game c _ r) = Game c i r
inputGame _ g = g

stepGame :: Clock -> Game -> Game
stepGame c (Game _ i r) = Game c i r
stepGame _  g = g

renderGame :: Game -> IO Game
renderGame g@(Game c i r) = do
    clear [ColorBuffer, DepthBuffer]
    renderEvents $ i ^. events
    let names = ["projection","sampler"]
        ups   = [matUp, samUp]
        matUp l = glUniformMatrix4fv l 1 1
        samUp l _ = glUniform1i l 0
        arrs  = map concat [proj]
        proj  = orthoMatrix 0 w 0 h 0 1 :: Matrix GLfloat
        (w,h) = (fromIntegral wi, fromIntegral hi)
        (wi,hi) = i ^. state . windowSize
        fps = show (ceiling $ c ^. avgFPS) ++ " fps"
    updateUniforms names ups arrs
    renderString fps (0,0) (16,16) r
    return g
renderGame g = return g

renderEvents :: [InputEvent] -> IO ()
renderEvents = mapM_ renderEvent

renderEvent :: InputEvent -> IO ()
renderEvent (WindowSizeChangedTo (w,h)) = viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
renderEvent _ = return ()

endGame :: t -> IO ()
endGame _ = putStrLn "Done."

makeGameRenderable :: ResourceStore -> Renderable
makeGameRenderable rs =
    Renderable ps bs ts [] (\_ -> return ())
        where ps = maybe [] (\p -> [_program p]) (M.lookup "texShader" $ rs ^. programMap)
              bs = maybeToList (M.lookup "text" $ rs ^. vboMap)
              ts = maybeToList (M.lookup "text" $ rs ^. textureMap)

renderString :: String -> (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> Renderable -> IO ()
renderString s p (w,h) r = zipWithM_ renderChar' s ps
    where renderChar' c p' = renderChar c p' (w,h) r
          (_,ps) = foldl accum' (p,[]) s
          accum' ((w',h'),l) c = if c == '\n'
                                 then ((w, h'+h),l ++ [(w,h'+h)])
                                 else ((w'+w, h'),l ++ [(w'+w,h')])


renderChar :: Char -> (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> Renderable -> IO ()
renderChar ' ' _ _ _ = return ()
renderChar c (x,y) (w,h) (Renderable (p:_) (v:_) (t:_) _ _) = do
    let names = ["modelview"]
        matUp l = glUniformMatrix4fv l 1 1
        ups  = [matUp]
        modv = applyTransformation tfrm $ identityN 4 :: Matrix GLfloat
        tfrm = (Rotation 0 0 0, Scale w h 1, Translation x y 0)
        arrs = [concat modv]
        ndx  = fromIntegral $ fromEnum c - 33

    currentProgram $= Just p
    updateUniforms names ups arrs
    activeTexture $= TextureUnit 0
    textureBinding Texture2D $= Just t
    bindInterleavedVbo v
    drawArrays TriangleFan (4*ndx) 4

renderChar _ _ _ _ = return ()

renderText :: t -> a
renderText _ = undefined


