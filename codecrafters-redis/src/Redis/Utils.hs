module Redis.Utils (
    myTracePretty,
    myTracePrettyM,
    fromEither,
    mapLeft,
    millisecondsToSeconds,
    secondsToMilliseconds,
    convergeEither,
    toUpperBs,
    showUsingBase,
    combineDecimalDigits,
) where

import Data.ByteString.Char8 qualified as BS

import Data.ByteString (ByteString)
import Data.Char (intToDigit, toUpper)
import Debug.Pretty.Simple

myTracePretty :: (Show a) => String -> a -> a
myTracePretty str' a = pTrace (str' <> show a) a

myTracePrettyM :: (Show a, Applicative f) => String -> a -> f ()
myTracePrettyM str a = pTraceM (str <> show a)

fromEither :: Either a a -> a
fromEither = either id id

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f = either (Left . f) Right

-- Using fractional division to avoid losing precision.
-- Integer division would round 0.1 to 0 which we do not want, we want to have division results as is with little to no rounding
millisecondsToSeconds :: (Fractional a) => a -> a
millisecondsToSeconds = (/ 1000)

secondsToMilliseconds :: (Num a) => a -> a
secondsToMilliseconds = (* 1000)

convergeEither :: (a -> b) -> Either a a -> b
convergeEither f = either f f

toUpperBs :: ByteString -> ByteString
toUpperBs = BS.map toUpper

showUsingBase :: Int -> Int -> String
showUsingBase base num = go num ""
  where
    go v = case v `divMod` base of
        (0, r) -> showChar (intToDigit r)
        (d, r) -> go d . showChar (intToDigit r)

combineDecimalDigits :: [Int] -> Int
combineDecimalDigits [] = 0
combineDecimalDigits digits = sum $ zipWith (\digit numOfZeros -> digit * (10 ^ numOfZeros)) (reverse digits) [0 ..]
