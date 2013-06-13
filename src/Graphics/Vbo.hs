{-# LANGUAGE RecordWildCards #-}
module Graphics.Vbo where

import Graphics.Util
import Graphics.Rendering.OpenGL

import Foreign.Marshal.Array ( withArray )
import Foreign.Storable      ( sizeOf, Storable )
import Foreign.Ptr           ( Ptr, nullPtr, plusPtr )
import Control.Monad         ( zipWithM, void )

-- | A vertex buffer object with interleaved array elements.
-- data Vbo = IVbo { vboSizes  :: [NumComponents]  -- ^ The number of compontents of each vertex attribute.
--                  , vboLocs   :: [AttribLocation] -- ^ The locations of the vertex attribute.
--                  , vboBuffer :: BufferObject     -- ^ The vbo ids.
--                  , vboStride :: Stride           -- ^ The stride of all components of one vertex.
--                  }
--
-- data Vbo = Vbo { vboBuffer :: BufferObject
data InterleavedVbo = InterleavedVbo { vboLocs  :: [AttribLocation]
                                     , vboDescs :: [VertexArrayDescriptor Int]
                                     , vboBuff  :: BufferObject
                                     }

createVbo :: Storable a => [a] -> IO BufferObject   -- ^ A vertex buffer object.
createVbo vertexData = do
    [vbo] <- genObjectNames 1
    bindBuffer ArrayBuffer $= Just vbo
    let floatSize = length vertexData * sizeOf (undefined :: Float)
    withArray vertexData $ \ptr ->
        bufferData ArrayBuffer $= (fromIntegral floatSize, ptr, StaticDraw)
    vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 3 Float 0 nullPtr)
    return vbo

interleavedVbo :: (Storable a, Show a) => [[a]]
           -> [NumComponents]
           -> [AttribLocation]
           -> IO InterleavedVbo
interleavedVbo arrays sizes locs = do
    putStrLn "Creating interleaved vbo."

    [vbo] <- genObjectNames 1
    bindBuffer ArrayBuffer $= Just vbo

    mapM_ (\loc -> vertexAttribArray loc $= Enabled) locs

    let ilvd     = interleaveArrayData arrays sizes
        floatSize= sizeOf (undefined :: Float)
        buffSize = length ilvd * floatSize
        offsets  = componentOffsets sizes
        sAndOs   = zip sizes offsets -- Sizes and offsets.
        stride   = fromIntegral floatSize * sum (map fromIntegral sizes)

    withArray ilvd $ \ptr ->
        bufferData ArrayBuffer $= (fromIntegral buffSize, ptr, StaticDraw)
    descs <- zipWithM (\loc (size, offset) -> let desc = VertexArrayDescriptor size Float stride offset in do
        vertexAttribPointer loc $= (ToFloat, desc)
        return desc) locs sAndOs
    printError
    putStrLn "Defined interleaved array pointers."
    return InterleavedVbo { vboLocs  = locs
                          , vboDescs = descs
                          , vboBuff  = vbo
                          }

-- | Takes a list of NumComponents and creates their offsets.
componentOffsets :: [NumComponents] -- ^ The compontents.
                 -> [Ptr Int]       -- ^ Their offsets.
componentOffsets sizes =
    let sizeInBytes      = (fromIntegral floatSize  *)
        floatSize        = sizeOf (undefined :: Float)
        limitToLen       = take . length
        accumulate acc s = acc + fromIntegral s
    in
    limitToLen sizes $ map (plusPtr nullPtr) $ scanl accumulate 0 $ map sizeInBytes sizes

-- | Takes a list of arrays and interleaves them according to given component sizes.
interleaveArrayData :: [[a]]    -- ^ The array data.
                    -> [GLint]  -- ^ The sizes of each component of a vertex.
                    -> [a]      -- ^ The interleaved array data.

interleaveArrayData arrays sizes = if all (not . null) arrays then
   let takes  = fmap (take . fromIntegral) sizes -- Get our 'take' functions.
       drops  = fmap (drop . fromIntegral) sizes -- Get our 'drop' functions.
       chunks = zipWith ($) takes arrays  -- The head vertex components.
       rest   = zipWith ($) drops arrays in
   concat chunks ++ interleaveArrayData rest sizes
   else []

bindVbo :: GLint        -- ^ The number of components in this attribute.
        -> GLuint       -- ^ The location of the array.
        -> BufferObject -- ^ The vbo buffer object.
        -> IO ()

bindVbo size loc vb = do
    vertexAttribArray (AttribLocation loc) $= Enabled
    bindBuffer ArrayBuffer $= Just vb
    vertexAttribPointer (AttribLocation loc) $= (ToFloat, VertexArrayDescriptor size Float 0 nullPtr)

bindInterleavedVbo :: InterleavedVbo -> IO ()
bindInterleavedVbo InterleavedVbo{..} = do
    bindBuffer ArrayBuffer $= Just vboBuff
    void $ zipWithM (\loc desc -> do
       vertexAttribPointer loc $= (ToFloat, desc)
       vertexAttribArray loc   $= Enabled) vboLocs vboDescs

