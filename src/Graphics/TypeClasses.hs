module Graphics.TypeClasses where

import Graphics.Rendering.OpenGL ( GLfloat )

class Transformable a where
    rotate    :: GLfloat -> GLfloat -> GLfloat -> a -> a
    scale     :: GLfloat -> GLfloat -> GLfloat -> a -> a 
    translate :: GLfloat -> GLfloat -> GLfloat -> a -> a 

