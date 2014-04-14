module Tiempo.Concurrent (threadDelay, timeout) where

import qualified Control.Concurrent as Concurrent (threadDelay)
import qualified System.Timeout     as Timeout (timeout)

import Tiempo (TimeInterval, toMicroSeconds)

-- | Like @Control.Concurrent.threadDelay@ but accepts a
-- @TimeInterval@ as an argument instead of an Int
threadDelay :: TimeInterval -> IO ()
threadDelay = Concurrent.threadDelay . toMicroSeconds
{-# INLINE threadDelay #-}


-- | Like @System.Timeout@ but accepts a @TimeInterval@ as an argument
-- instead of an Int
timeout :: TimeInterval -> IO a -> IO (Maybe a)
timeout = Timeout.timeout . toMicroSeconds
{-# INLINE timeout #-}
