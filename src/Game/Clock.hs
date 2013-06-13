{-# LANGUAGE CPP #-}
module Game.Clock where

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
