module Redis.Utils (
    myTrace,
    myTraceM,
    fromEither,
    mapLeft,
    millisecondsToSeconds,
    secondsToMilliseconds,
    convergeEither,
) where

import Debug.Trace (trace, traceShowM)

myTrace :: (Show a) => String -> a -> a
myTrace str' a = trace (str' <> show a) a

myTraceM :: (Show a, Applicative f) => String -> a -> f ()
myTraceM str a = traceShowM (str <> show a)

fromEither :: Either a a -> a
fromEither = either id id

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f = either (Left . f) Right

millisecondsToSeconds :: (Integral a) => a -> a
millisecondsToSeconds = (`div` 1000)

secondsToMilliseconds :: (Integral a) => a -> a
secondsToMilliseconds = (* 1000)

convergeEither :: (a -> b) -> Either a a -> b
convergeEither f = either f f
