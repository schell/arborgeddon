module Graphics.Texture where

import Graphics.Util
import Codec.Picture
import Graphics.Rendering.OpenGL.GL
import Control.Monad                ( when, unless, foldM )
import Data.Vector.Storable         ( unsafeWith )
import Data.Maybe                   ( isNothing )
import qualified Data.Map as M


type TextureMap = M.Map String (Maybe TextureObject)

initTextures :: [FilePath] -- ^ A list of image files to use as textures.
             -> IO TextureMap
initTextures files = do
    -- Turn on texturing.
    texture Texture2D $= Enabled
    (_, textures') <- foldM accumulateTextures acc files
    return textures'
        where acc = (0, M.empty)

accumulateTextures :: (Int, TextureMap) -> FilePath -> IO (Int, TextureMap)
accumulateTextures (n,m) file = do
    mTex <- initTexture file n
    return (n+1, M.insert file mTex m)

initTexture :: FilePath -- ^ The texture to load.
            -> Int      -- ^ The index of the texture unit to hold the texture in.
            -> IO (Maybe TextureObject)
initTexture file u = do
    -- Load our texture or die.
    mTex <- loadTexture file u
    unless (isNothing mTex) $ do
        -- Set the texture params on our bound texture.
        textureFilter   Texture2D   $= ((Nearest, Nothing), Nearest)
        textureWrapMode Texture2D S $= (Repeated, Clamp)
        textureWrapMode Texture2D T $= (Repeated, Clamp)
    when (isNothing mTex) $ putStrLn $ "Could not initialize "++ file
    return mTex

loadTexture :: FilePath -> Int -> IO (Maybe TextureObject)
loadTexture f u = do
    putStrLn $ "Loading texture "++f
    eDynImg <- readImage f
    case eDynImg of
        Left note  -> do
            putStrLn $ "Could not load texture '"++f++"'.\nNote: "++note
            return Nothing

        Right img -> do
            -- Get our texture object.
            tex  <- newBoundTexUnit u
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
