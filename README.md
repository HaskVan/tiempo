# tiempo

A sane and simple API that sits on top of the `time` library; it
allows the creation of time intervals, and functions to manipulate
time using them. Inspired on the `Time` module found on
[distributed-process-platform](http://github.com/haskell-distributed/distributed-process-platform).

Example:

```haskell
import qualified Control.Concurrent as Concurrent (threadDelay)
import           Data.Time          (UTCTime, addUTCTime, getCurrentTime)
import           Tiempo             (TimeInterval, days, hours, minutes,
                                     seconds, toMicroSeconds, toNominalDiffTime,
                                     toSeconds)

threadDelay :: TimeInterval -> IO ()
threadDelay = Concurrent.threadDelay . toMicroSeconds

fromTime :: TimeInterval ->  UTCTime -> UTCTime
fromTime interval = addUTCTime (toNominalDiffTime interval)

fromNow :: TimeInterval -> IO UTCTime
fromNow interval = fromTime interval `fmap` getCurrentTime

agoTime :: TimeInterval -> UTCTime -> UTCTime
agoTime interval = addUTCTime (realToFrac (-1 * toSeconds interval))

ago :: TimeInterval -> IO UTCTime
ago interval = agoTime interval `fmap` getCurrentTime

main :: IO ()
main = do
  ago (minutes 3) >>= print
  getCurrentTime >>= print
  fromNow (minutes 3) >>= print
  fromNow (hours 4) >>= print
  fromNow (days 1) >>= print
  threadDelay $ seconds 5
  putStrLn "DONE"
```
