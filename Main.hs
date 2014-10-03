{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
module Main where

import Arborgeddon.Types
import Arborgeddon.Rendering
import Arborgeddon.Yarn.Event
import Gelatin
import Yarn
import System.Exit
import System.Directory
import System.FilePath ((</>))
import Data.IORef
import Data.Time.Clock
import Data.Traversable
import Control.Monad hiding (forever)
import Control.Monad.Reader hiding (forever)
import Control.Applicative
import Control.Arrow hiding (loop)

type App = ([Colored [V2 Double]], [Colored AABB], [Colored (V2 Double, V2 Double)])

aabbToLines :: AABB -> [V2 Double]
aabbToLines a = [V2 l t, V2 r t, V2 r b, V2 l b, V2 l t]
    where (l,t,r,b) = edgesAABB a

rnormalOf :: V2 Double -> V2 Double
rnormalOf (V2 x y) = V2 (-y) x

lnormalOf :: V2 Double -> V2 Double
lnormalOf (V2 x y) = V2 y (-x)

projectionOnto :: V2 Double -> V2 Double -> V2 Double
projectionOnto a@(V2 ax ay) b@(V2 bx by) = V2 x y
    where x = b2 * bx
          y = b2 * by
          b2 = dp / (bx*bx + by*by)
          dp = a `dot` b

insideAABB :: V2 Double -> AABB -> Bool
insideAABB (V2 px py) (AABB (V2 x y) hw hh) =
    px > (x - hw) && px < (x + hw) && py > (y - hh) && py < (y + hh)

intersectsAABB :: AABB -> AABB -> Bool
intersectsAABB a b =
    (l1 < r2 && r1 > l2) && (t1 < b2 && b1 > t2) ||
    (l2 < r1 && r2 > l1) && (t2 < b1 && b2 > t1)
        where (l1,t1,r1,b1) = edgesAABB a
              (l2,t2,r2,b2) = edgesAABB b

edgesAABB :: AABB -> (Double,Double,Double,Double)
edgesAABB (AABB (V2 x y) hw hh) = (x - hw, y - hh, x + hw, y + hh)

intersectionAABB :: AABB -> AABB -> Maybe AABB
intersectionAABB a b = if a `intersectsAABB` b then Just c else Nothing
    where c  = AABB v hw hh
          v  = V2 (l + hw) (t + hh)
          hw = (r - l)/2
          hh = (bm - t)/2
          l  = max l1 l2
          r  = min r1 r2
          t  = max t1 t2
          bm = min b1 b2
          (l1,t1,r1,b1) = edgesAABB a
          (l2,t2,r2,b2) = edgesAABB b

draggableBox :: AABB -> YarnR a AABB
draggableBox (AABB sv hw hh) = AABB <$> clickDrag sv <*> pure hw <*> pure hh
    where mousedDownInAABB b = (*>) <$> cursorIsInAABB b <*> mousedDown
          clickDrag v = (pure v `untilWithE`) (mousedDownInAABB (AABB v hw hh)) $ \vdown ->
                            (fmap (v - vdown +) <$> mouseIsDown) `andThenWith` \(Just v') ->
                                clickDrag v'

-- | The box used to rotate the projection axis.
boxA :: YarnR a AABB
boxA = draggableBox $ AABB (V2 100 100) 50 100

boxB :: YarnR a AABB
boxB = draggableBox $ AABB (V2 200 200) 100 50

offsetAABBs :: YarnR (AABB,AABB) (V2 Double)
offsetAABBs = proc (a,b) -> do
    returnA -< (aabbCenter b - aabbCenter a)

cursorIsInAABB b = (`insideAABB` b) <$> cursorPos ~> triggerTrue

highlightedBox :: V4 Double -> V4 Double -> YarnR AABB (Colored AABB)
highlightedBox clrOff clrOn = proc a -> do
    p <- cursorPos -< ()
    let c = if p `insideAABB` a then clrOn else clrOff
    returnA -< Colored a c

twoAABBs :: YarnR a [Colored AABB]
twoAABBs = sequenceA [a,b]
    where a = boxA ~> (coloredWith $ alpha myRed 0.5)
          b = boxB ~> (coloredWith $ alpha myBlue 0.5)
          coloredWith c = yarn $ (flip Colored c)

myRed = V4 0.53 0.13 0.13 1
myBlue = V4 0.13 0.13 0.53 1

lines' :: YarnR a [Colored [V2 Double]]
lines' = sequenceA [dcenters, aLines, bLines]
    where dcenters = proc _ -> do (a:b:_) <- twoAABBs -< ()
                                  returnA -< Colored [aabbCenter $ uncolored a, aabbCenter $ uncolored b] gray
          aLines = boxA ~> (yarn $ \a -> Colored (aabbToLines a) myRed)
          bLines = boxB ~> (yarn $ \b -> Colored (aabbToLines b) myBlue)

arrows :: YarnR a [Colored (V2 Double, V2 Double)]
arrows = sequenceA [dcenters]
    where dcenters = proc _ -> do (a:b:_) <- twoAABBs -< ()
                                  returnA -< Colored (aabbCenter $ uncolored a, aabbCenter $ uncolored b) black

aabbs :: YarnR a [Colored AABB]
aabbs = twoAABBs

main :: IO ()
main = do
    wref <- initWindow (V2 0 0) (V2 600 480) "Arborgeddon"
    rndr <- mkArborenderer
    t    <- getCurrentTime
    loop t ((,,) <$> lines' <*> aabbs <*> arrows) rndr wref emptyInputEnv

loop :: UTCTime -> Yarn (Reader InputEnv) () App -> Arborenderer -> WindowRef -> InputEnv -> IO ()
loop t y rndr wref env = do
    pollEvents
    (events, window) <- readIORef wref
    let env' = foldl foldInput env events
    writeIORef wref ([], window)

    (width, height) <- getWindowSize window
    t' <- getCurrentTime
    let dt = realToFrac $ diffUTCTime t' t
        Output (lns, boxes, ars) y' = runReader (stepYarn y dt ()) env'

    makeContextCurrent $ Just window

    (clearFrame rndr) width height
    forM_ boxes $ drawAABB rndr
    forM_ lns $ drawLines rndr
    forM_ ars $ drawArrow rndr

    swapBuffers window
    shouldClose <- windowShouldClose window
    when shouldClose exitSuccess
    loop t' y' rndr wref $ clearEvents env'


