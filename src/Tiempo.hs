{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Tiempo
  ( TimeInterval
  -- * TimeInterval to different units and types
  , toMicroSeconds
  , toMilliSeconds
  , toSeconds
  , toMinutes
  , toHours
  , toDays
  , toNominalDiffTime

  -- * Time manipulation
  , fromTime
  , fromNow
  , agoTime
  , ago

  -- * Unit functions
  , microSeconds
  , milliSeconds
  , seconds
  , minutes
  , hours
  , days ) where

import Control.DeepSeq (NFData (..))
import Data.Time       (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Data.Typeable   (Typeable)
import GHC.Generics    (Generic)

--------------------------------------------------------------------------------

data  TimeUnit
  = Days | Hours | Minutes | Seconds | Millis | Micros
  deriving (Eq, Show, Typeable, Generic)

instance NFData TimeUnit

data TimeInterval
  = TimeInterval !TimeUnit !Int
  deriving (Eq, Show, Typeable, Generic)

instance NFData TimeInterval where
  rnf (TimeInterval unit n) =
    unit `seq` n `seq` ()

--------------------------------------------------------------------------------

-- | converts the supplied @TimeInterval@ to microseconds
toMicroSeconds :: TimeInterval -> Int
toMicroSeconds (TimeInterval u v) = timeToMicros u v
{-# INLINE toMicroSeconds #-}

-- | converts the supplied @TimeInterval@ to milliseconds
toMilliSeconds :: TimeInterval -> Double
toMilliSeconds (TimeInterval u v) = timeToMillis u v
{-# INLINE toMilliSeconds #-}

-- | converts the supplied @TimeInterval@ to seconds
toSeconds :: TimeInterval -> Double
toSeconds (TimeInterval u v) = timeToSeconds u v
{-# INLINE toSeconds #-}

-- | converts the supplied @TimeInterval@ to minutes
toMinutes :: TimeInterval -> Double
toMinutes (TimeInterval u v) = timeToMinutes u v
{-# INLINE toMinutes #-}

-- | converts the supplied @TimeInterval@ to hours
toHours :: TimeInterval -> Double
toHours (TimeInterval u v) = timeToHours u v
{-# INLINE toHours #-}


-- | converts the supplied @TimeInterval@ to days
toDays :: TimeInterval -> Double
toDays (TimeInterval u v) = timeToDays u v
{-# INLINE toDays #-}

-- | converts the supplied @TimeInterval@ to NominalDiffTime
toNominalDiffTime :: TimeInterval -> NominalDiffTime
toNominalDiffTime (TimeInterval u v) = realToFrac $ timeToSeconds u v
{-# INLINE toNominalDiffTime #-}

--------------------------------------------------------------------------------

-- | Adds @TimeInterval@ to @UTCTime@ returning a @UTCTime@ in the
-- future
fromTime :: TimeInterval ->  UTCTime -> UTCTime
fromTime interval = addUTCTime (toNominalDiffTime interval)
{-# INLINE fromTime #-}

-- | Adds @TimeInterval@ to @getCurrentTime@ returning a
-- @UTCTime@ in the future
fromNow :: TimeInterval -> IO UTCTime
fromNow interval = fromTime interval `fmap` getCurrentTime
{-# INLINE fromNow #-}

-- | Substracts @TimeInterval@ from @UTCTime@ returning a @UTCTime@ in
-- the past
agoTime :: TimeInterval -> UTCTime -> UTCTime
agoTime interval = addUTCTime (realToFrac (-1 * toSeconds interval))
{-# INLINE agoTime #-}

-- | Substracts @TimeInterval@ from @getCurrentTime@ returning a
-- @UTCTime@ in the past
ago :: TimeInterval -> IO UTCTime
ago interval = agoTime interval `fmap` getCurrentTime
{-# INLINE ago #-}

--------------------------------------------------------------------------------

-- | given a number, produces a @TimeInterval@ of microseconds
microSeconds :: Int -> TimeInterval
microSeconds = TimeInterval Micros
{-# INLINE microSeconds #-}

-- | given a number, produces a @TimeInterval@ of milliseconds
milliSeconds :: Int -> TimeInterval
milliSeconds = TimeInterval Millis
{-# INLINE milliSeconds #-}

-- | given a number, produces a @TimeInterval@ of seconds
seconds :: Int -> TimeInterval
seconds = TimeInterval Seconds
{-# INLINE seconds #-}

-- | given a number, produces a @TimeInterval@ of minutes
minutes :: Int -> TimeInterval
minutes = TimeInterval Minutes
{-# INLINE minutes #-}

-- | given a number, produces a @TimeInterval@ of hours
hours :: Int -> TimeInterval
hours = TimeInterval Hours
{-# INLINE hours #-}

-- | given a number, produces a @TimeInterval@ of days
days :: Int -> TimeInterval
days = TimeInterval Days
{-# INLINE days #-}
--------------------------------------------------------------------------------

-- | converts the supplied @TimeUnit@ to microseconds
timeToMicros :: TimeUnit -> Int -> Int
timeToMicros Micros us = us
timeToMicros Millis ms = ms * (10 ^ (3 :: Int)) -- (1000µs == 1ms)
timeToMicros Seconds secs = timeToMicros Millis (secs * milliSecondsPerSecond)
timeToMicros Minutes mins = timeToMicros Seconds (mins * secondsPerMinute)
timeToMicros Hours hrs = timeToMicros Minutes (hrs * minutesPerHour)
timeToMicros Days dys = timeToMicros Hours (dys * hoursPerDay)
{-# INLINE timeToMicros #-}

timeToMillis :: TimeUnit -> Int -> Double
timeToMillis Micros us = fromIntegral us / fromIntegral microSecondsPerMillis
timeToMillis Millis ms = fromIntegral ms
timeToMillis Seconds secs = fromIntegral secs * (10 ^ (3 :: Int))
timeToMillis Minutes mins = timeToMillis Seconds (mins * secondsPerMinute)
timeToMillis Hours hrs = timeToMillis Minutes (hrs * minutesPerHour)
timeToMillis Days dys = timeToMillis Hours (dys * hoursPerDay)
{-# INLINE timeToMillis #-}

timeToSeconds :: TimeUnit -> Int -> Double
timeToSeconds Micros us =
  fromIntegral us / fromIntegral (microSecondsPerMillis * milliSecondsPerSecond)
timeToSeconds Millis ms = fromIntegral ms / fromIntegral milliSecondsPerSecond
timeToSeconds Seconds secs = fromIntegral secs
timeToSeconds Minutes mins = fromIntegral $ mins * secondsPerMinute
timeToSeconds Hours hrs = timeToSeconds Minutes (hrs * minutesPerHour)
timeToSeconds Days dys = timeToSeconds Hours (dys * hoursPerDay)
{-# INLINE timeToSeconds #-}


timeToMinutes :: TimeUnit -> Int -> Double
timeToMinutes Micros us =
    fromIntegral us / fromIntegral microsPerHour
  where
    microsPerHour = product [ microSecondsPerMillis
                            , milliSecondsPerSecond
                            , secondsPerMinute ]
timeToMinutes Millis ms = fromIntegral ms / fromIntegral microsPerMin
  where
    microsPerMin = product [ milliSecondsPerSecond
                           , secondsPerMinute ]
timeToMinutes Seconds secs = fromIntegral secs / fromIntegral secondsPerMinute
timeToMinutes Minutes mins = fromIntegral mins
timeToMinutes Hours hrs = timeToMinutes Minutes (hrs * minutesPerHour)
timeToMinutes Days dys = timeToMinutes Hours (dys * hoursPerDay)
{-# INLINE timeToMinutes #-}


timeToHours :: TimeUnit -> Int -> Double
timeToHours Micros us = fromIntegral us / fromIntegral microsPerHour
  where
    microsPerHour = product [ microSecondsPerMillis
                            , milliSecondsPerSecond
                            , secondsPerMinute
                            , minutesPerHour ]
timeToHours Millis ms = fromIntegral ms / fromIntegral millisPerHour
  where
    millisPerHour = product [ milliSecondsPerSecond
                            , secondsPerMinute
                            , minutesPerHour ]
timeToHours Seconds secs =
  fromIntegral secs / fromIntegral (secondsPerMinute * minutesPerHour)
timeToHours Minutes mins =
  fromIntegral mins / fromIntegral minutesPerHour
timeToHours Hours hrs = fromIntegral hrs
timeToHours Days dys = timeToHours Hours (dys * hoursPerDay)
{-# INLINE timeToHours #-}


timeToDays :: TimeUnit -> Int -> Double
timeToDays Micros us = fromIntegral us / fromIntegral microsPerDay
  where
    microsPerDay = product [ microSecondsPerMillis
                           , milliSecondsPerSecond
                           , secondsPerMinute
                           , minutesPerHour
                           , hoursPerDay ]
timeToDays Millis ms = fromIntegral ms / fromIntegral millisPerDay
  where
    millisPerDay = product [ milliSecondsPerSecond
                           , secondsPerMinute
                           , minutesPerHour
                           , hoursPerDay ]
timeToDays Seconds secs = fromIntegral secs / fromIntegral secondsPerDay
  where
    secondsPerDay = product [ secondsPerMinute
                            , minutesPerHour
                            , hoursPerDay ]
timeToDays Minutes mins =
  fromIntegral mins / fromIntegral (minutesPerHour * hoursPerDay)
timeToDays Hours hrs = fromIntegral $ hrs * hoursPerDay
timeToDays Days dys = fromIntegral dys
{-# INLINE timeToDays #-}

--------------------------------------------------------------------------------

hoursPerDay :: Int
hoursPerDay = 24
{-# INLINE hoursPerDay #-}

minutesPerHour :: Int
minutesPerHour = 60
{-# INLINE minutesPerHour #-}

secondsPerMinute :: Int
secondsPerMinute = 60
{-# INLINE secondsPerMinute #-}

milliSecondsPerSecond :: Int
milliSecondsPerSecond = 1000
{-# INLINE milliSecondsPerSecond #-}

microSecondsPerMillis :: Int
microSecondsPerMillis = 1000
{-# INLINE microSecondsPerMillis #-}
