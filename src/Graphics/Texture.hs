module Graphics.Texture where

import Graphics.Util
import Codec.Picture

import Data.Vector.Storable                                         ( unsafeWith )
import Graphics.Rendering.OpenGL.GL

loadTexture :: FilePath -> IO (Maybe TextureObject)
loadTexture f = do
    putStrLn $ "Loading texture "++f
    eDynImg <- readImage f
    case eDynImg of
        Left note  -> do
            putStrLn $ "Could not load texture '"++f++"'.\nNote: "++note
            return Nothing

        Right (ImageRGBA8 (Image w h dat)) -> do
            -- Make our texture name.
            [tex] <- genObjectNames 1
            texture Texture2D $= Enabled
            activeTexture     $= TextureUnit 0
            textureBinding Texture2D $= Just tex
            -- Buffer our texture data.
            unsafeWith dat $ \ptr ->
                texImage2D
                  -- No cube map
                  Nothing
                  -- No proxy
                  NoProxy
                  -- No mipmaps
                  0
                  -- Internal storage @ rgba8
                  RGBA8
                  -- Size of the image
                  (TextureSize2D (fromIntegral w) (fromIntegral h))
                  -- No borders
                  0
                  -- Pixel data in unsigned bytes, rgba order
                  (PixelData RGBA UnsignedByte ptr)
            printError
            return $ Just tex
        _ -> do
            putStrLn $ "Texture '"++f++"' is not an expected format (expecting RGBA8)."
            return Nothing

