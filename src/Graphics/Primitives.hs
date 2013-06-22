module Graphics.Primitives where

triangle :: Num a => [a]
triangle = [  0,  1, 0
           , -1, -1, 0
           ,  1, -1, 0
           ]

square :: Num a => [a]
square = [ -1,  1, 0
         , -1, -1, 0
         ,  1, -1, 0
         ,  1, -1, 0
         , -1,  1, 0
         ,  1,  1, 0
         ]
