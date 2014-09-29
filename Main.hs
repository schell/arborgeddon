{-# LANGUAGE FlexibleContexts #-}
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

insideAABB :: V2 Double -> AABB -> Bool
insideAABB (V2 px py) (AABB (V2 x y) hw hh) =
    px > (x - hw) && px < (x + hw) && py > (y - hh) && py < (y + hh)

boxA :: YarnR a AABB
boxA = AABB <$> clickDrag startingPos <*> pure hw <*> pure hh
    where startingPos = V2 300 300
          cursorIsInAABB b = (`insideAABB` b) <$> cursorPos ~> triggerTrue
          mousedDownInAABB b = (*>) <$> cursorIsInAABB b <*> mousedDown
          clickDrag v = (pure v `untilWithE`) (mousedDownInAABB (AABB v hw hh)) $ \vdown ->
                            (fmap (v - vdown +) <$> mouseIsDown) `andThenWith` \(Just v') ->
                                clickDrag v'
          (hw,hh) = (100, 50)

twoAABBs :: YarnR a [AABB]
twoAABBs = sequenceA [topleft]
    where topleft = boxA
          --btmrite = pure $ AABB (V2 500 380) 100 50

hoveredAABB :: YarnR a [AABB]
hoveredAABB = (,) <$> twoAABBs <*> cursorPos ~> yarn (uncurry reduce)
    where reduce as p = filter (insideAABB p) as

aabbs :: YarnR a [AABB]
aabbs = twoAABBs

main :: IO ()
main = do
    wref <- initWindow (V2 0 0) (V2 600 480) "Arborgeddon"
    rndr <- mkArborenderer
    t    <- getCurrentTime
    loop t aabbs rndr wref emptyInputEnv

loop :: UTCTime -> Yarn (Reader InputEnv) () [AABB] -> Arborenderer -> WindowRef -> InputEnv -> IO ()
loop t y rndr wref env = do
    pollEvents
    (events, window) <- readIORef wref
    let env' = foldl foldInput env events
    writeIORef wref ([], window)

    (width, height) <- getWindowSize window
    t' <- getCurrentTime
    let dt = realToFrac $ diffUTCTime t' t
        Output boxes y' = runReader (stepYarn y dt ()) env'

    makeContextCurrent $ Just window

    (clearFrame rndr) width height
    mapM_ (drawAABB rndr) boxes

    swapBuffers window
    shouldClose <- windowShouldClose window
    when shouldClose exitSuccess
    loop t' y' rndr wref $ clearEvents env'


