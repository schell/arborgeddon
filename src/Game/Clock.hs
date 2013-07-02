{-# LANGUAGE CPP, TemplateHaskell #-}
module Game.Clock where

import Control.Lens
import Control.Monad.State

#ifdef darwin_HOST_OS

import Control.Applicative ( (<$>) )
import Data.Time.Clock.POSIX

getTime :: IO Double
getTime = realToFrac <$> getPOSIXTime

#else

import Control.Applicative ( (<$>) )
import System.Clock

getTime :: IO Double
getTime =
    toSeconds <$> getTime Monotonic
  where
    toSeconds (TimeSpec seconds nanos) =
        fromIntegral seconds + fromIntegral nanos * 10**(-9)

#endif

data Clock = Clock { _frames   :: [Double]
                   , _avgFPS   :: Double
                   , _timeNow  :: Double
                   , _timePrev :: Double
                   }
makeLenses ''Clock

emptyClock :: Clock
emptyClock = Clock (replicate 100 0) 0.0 0.0 0.0

tickClock :: Double -> State Clock ()
tickClock t = do
    fs   <- use frames
    prev <- use timeNow
    let len  = length fs
        dt   = t - prev

    timePrev .= prev
    timeNow  .= t
    frames   .= take len (dt:fs)
    avgFPS   .= 1/(sum fs /fromIntegral len)

