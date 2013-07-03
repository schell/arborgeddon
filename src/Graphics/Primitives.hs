module Graphics.Primitives where

triangle3d :: Num a => [a]
triangle3d = [  0,  1, 0
             , -1, -1, 0
             ,  1, -1, 0
             ]

unitSquare3d :: Num a => [a]
unitSquare3d = [ -1,  1, 0
               , -1, -1, 0
               ,  1, -1, 0
               ,  1, -1, 0
               , -1,  1, 0
               ,  1,  1, 0
               ]

texSquare :: Num a => [a]
texSquare = [ 0, 0
            , 0, 1
            , 1, 1
            , 1, 1
            , 0, 0
            , 1, 0
            ]

texSquare3d :: Num a => [a]
texSquare3d = [ 0, 0, 0
              , 0, 1, 0
              , 1, 1, 0
              , 1, 1, 0
              , 0, 0, 0
              , 1, 0, 0
              ]


