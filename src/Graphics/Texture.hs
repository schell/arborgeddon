module Graphics.Texture where

import Graphics.Util
import Codec.Picture
import Graphics.Rendering.OpenGL.GL

import Control.Monad                ( unless )
import Data.Vector.Storable         ( unsafeWith )

loadTexture :: FilePath -> IO (Maybe TextureObject)
loadTexture f = do
    putStrLn $ "Loading texture "++f
    eDynImg <- readImage f
    case eDynImg of
        Left note  -> do
            putStrLn $ "Could not load texture '"++f++"'.\nNote: "++note
            return Nothing

        Right img -> do
            -- Get our texture object.
            tex  <- newBoundTexUnit 0
            -- Buffer our texture data.
            success <- bufferDataIntoBoundTexture img
            unless success $ putStrLn $ "    ("++f++")"
            return $ Just tex

newBoundTexUnit :: Int -> IO TextureObject
newBoundTexUnit u = do
    [tex] <- genObjectNames 1
    texture Texture2D $= Enabled
    activeTexture     $= TextureUnit (fromIntegral u)
    textureBinding Texture2D $= Just tex
    return tex

bufferDataIntoBoundTexture :: DynamicImage -> IO Bool
bufferDataIntoBoundTexture dynImg = case dynImg of
    (ImageRGB8 img)  -> unsafeTexImage2D RGB8 RGB img
    (ImageRGBA8 img) -> unsafeTexImage2D RGBA8 RGBA img
    _                -> do
        putStrLn "Texture is not an expected format (expecting RGB8 or RGBA8)."
        return False
    where unsafeTexImage2D rb r (Image w h dat) = do
            unsafeWith dat $ \ptr ->
                texImage2D
                  -- No cube map
                  Nothing
                  -- No proxy
                  NoProxy
                  -- No mipmaps
                  0
                  -- Internal storage @ rgba8
                  rb
                  -- Size of the image
                  (TextureSize2D (fromIntegral w) (fromIntegral h))
                  -- No borders
                  0
                  -- Pixel data in unsigned bytes, rgba order
                  (PixelData r UnsignedByte ptr)
            printError
            return True
