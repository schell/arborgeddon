module Main where

import App
import Game.Game
import Graphics
import Geometry
import Control.Lens
import Control.Monad
import Data.Monoid
import Data.Maybe
import Graphics.Rendering.OpenGL.Raw
import qualified Data.Map as M
import Graphics.Rendering.OpenGL hiding ( Matrix, Color )

main :: IO ()
main = do
    app <- initializeApp loadGame
    startApp app

type Color = (GLfloat, GLfloat, GLfloat, GLfloat)
--renderString :: String -> Color -> (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> Renderable -> IO ()
--renderString s c p (w,h) r = zipWithM_ renderChar' s ps
--    where renderChar' ch p' = renderChar ch c p' (w,h) r
--          (_,ps) = foldl accum' (p,[]) s
--          accum' ((w',h'),l) ch' = if ch' == '\n'
--                                   then ((w, h'+h),l ++ [(w,h'+h)])
--                                   else ((w'+w, h'),l ++ [(w'+w,h')])
--
--
--renderChar :: Char -> Color -> (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> Renderable -> IO ()
--renderChar ' ' _ _ _ _ = return ()
--renderChar ch c (x,y) (w,h) (Renderable (p:_) (v:_) (t:_) _) = do
--    let names = ["color","modelview"]
--        (r,g,b,a) = c
--        matUp l   = glUniformMatrix4fv l 1 1
--        colUp l   = glUniform4fv l 1
--        ups  = [colUp, matUp]
--        modv = applyTransformation tfrm $ identityN 4 :: Matrix GLfloat
--        tfrm = (Rotation 0 0 0, Scale w h 1, Translation x y 0)
--        arrs = [[r,g,b,a], concat modv]
--        ndx  = fromIntegral $ fromEnum ch - 33
--
--    currentProgram $= p ^. program
--    updateUniforms names ups arrs
--    activeTexture $= TextureUnit 0
--    textureBinding Texture2D $= Just t
--    bindInterleavedVbo v
--    drawArrays TriangleFan (4*ndx) 4

renderChar _ _ _ _ _ = return ()

