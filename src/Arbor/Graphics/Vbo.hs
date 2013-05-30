module Arbor.Graphics.Vbo where

import Arbor.Graphics.Util
import Graphics.Rendering.OpenGL

import Foreign.Marshal.Array ( withArray )
import Foreign.Storable      ( sizeOf, Storable )
import Foreign.Ptr           ( nullPtr )


createVbo :: Storable a => [a] -> IO BufferObject   -- ^ A vertex buffer object.
createVbo vertexData = do
    [vbo] <- genObjectNames 1
    bindBuffer ArrayBuffer $= Just vbo
    let floatSize = 3 * 3 * sizeOf (undefined :: Float)
    withArray vertexData $ \ptr ->
        bufferData ArrayBuffer $= (fromIntegral floatSize, ptr, StaticDraw)
    vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 3 Float 0 nullPtr)
    return vbo

bindVbo :: GLint        -- ^ The number of components in this attribute.
        -> GLuint       -- ^ The location of the array.
        -> BufferObject -- ^ The vbo buffer object.
        -> IO ()

bindVbo size loc vb = do
    vertexAttribArray (AttribLocation loc) $= Enabled
    bindBuffer ArrayBuffer $= Just vb
    vertexAttribPointer (AttribLocation loc) $= (ToFloat, VertexArrayDescriptor size Float 0 nullPtr)
