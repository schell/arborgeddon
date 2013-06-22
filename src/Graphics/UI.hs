module Graphics.UI where

import Geometry

data DisplayElement a = DisplayElement { _render    :: Matrix a -> IO ()
                                       , _children  :: [DisplayElement a]
                                       , _transform :: Transform3d a
                                       }


