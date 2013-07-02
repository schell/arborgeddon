{-# LANGUAGE TemplateHaskell #-}
module Graphics.Sprite where

import Geometry
import Graphics.Primitives
import Graphics.Vbo
import Graphics.Util
import Control.Lens
import Control.Monad.State
import Data.List ( intercalate )
import qualified Data.Map as M
import Graphics.Rendering.OpenGL hiding ( Matrix )

{- Sprites -}
data Sprite2d a = Sprite2d { _spriteRsrc :: Maybe a
                           , _spriteFPS  :: Double
                           , _spriteTick :: Double
                           , _frame      :: Int
                           , _boxes      :: [Rectangle GLfloat]
                           , _rsrcName   :: String
                           } deriving (Show, Eq)
makeLenses ''Sprite2d

-- | Increments a sprite's tick.
tickSprite :: (Show a, Eq a)
           => Double     -- ^ The change in seconds since the last call.
           -> Sprite2d a -- ^ The sprite to update.
           -> Sprite2d a
tickSprite dt sprite =
    let tick    = sprite^.spriteTick         -- Leftover time from last tick
        fps     = (sprite^.spriteFPS)          -- Frames per second
        total   = tick + dt                  -- Total time elapsed, leftover + dt
        finc    = total * fps                -- frames elapsed
        fint    = floor finc :: Int
        tick'   = (finc - fromIntegral fint) / fps-- leftover time this tick
        frame'  = sprite^.frame + fint
        nFrames = length (sprite^.boxes)
        frame'' = if nFrames == 0
                  then frame'
                  else frame' `mod` nFrames
    in flip execState sprite $ do
        frame .= frame''
        spriteTick .= tick'

initSprite :: M.Map String t -> Sprite2d (t, InterleavedVbo) -> IO (Sprite2d (t, InterleavedVbo))
initSprite m s = case M.lookup (s^.rsrcName) m of
    Nothing -> return s
    Just t  -> do
        let comps  = [3,2]
            attrbs = [AttribLocation 0, AttribLocation 1]
            boxs   = s^.boxes
            mScl   = scaleMatrix3d 32 32 1 :: Matrix GLfloat
            verts  = concat $ replicate (length boxs) $ transformVector3 mScl texSquare3d
            uvs    = toTriangles boxs
            datas  = [verts, uvs]
        ivbo <- interleavedVbo datas comps attrbs
        return s{_spriteRsrc = Just (t, ivbo)}

renderSprite :: Sprite2d (TextureObject, InterleavedVbo) -> IO ()
renderSprite s = case s^.spriteRsrc of
   Nothing   -> putStrLn $ showSprite s
   Just (t, ivbo) -> do
       activeTexture $= TextureUnit 0
       textureBinding Texture2D $= Just t
       printError
       bindInterleavedVbo ivbo
       let frame' = s^.frame
       drawArrays Triangles (fromIntegral frame'*6) 6

showSprite :: Sprite2d a -> String
showSprite s = "Sprite2d{" ++ intercalate ", " [ "spriteFPS = "  ++ show (s^.spriteFPS)
                                               --, "spriteTick = " ++ show (s^.spriteTick)
                                               , "frame = "      ++ show (s^.frame)
                                               --, "boxes = "      ++ show (s^.boxes)
                                               ] ++ "}"

makeFrames :: (Enum a, Num a, Fractional a)
           => a -- ^ Texture width
           -> a -- ^ Texture height
           -> a -- ^ Frame width
           -> a -- ^ Frame height
           -> a -- ^ Number of frames across
           -> a -- ^ Number of frames down
           -> [Rectangle a]
makeFrames tW tH fW fH fA fD = [ Rectangle (x*w,y*h) (w,h) | y <- [0..fD], x <- [0..fA] ]
    where w = fW/tW
          h = fH/tH
