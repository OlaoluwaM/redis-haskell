-- This is for the pTrace calls
{-# OPTIONS_GHC -Wno-warnings-deprecations #-}

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
    genericShow,
    inverseMap,
    universe,
    runReadM,
) where

import Data.ByteString.Char8 qualified as BS

import Control.Monad.Except (runExcept)
import Control.Monad.Reader (runReaderT)
import Data.ByteString (ByteString)
import Data.Char (intToDigit, toUpper)
import Data.Either (fromRight)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.String (IsString (..))
import Debug.Pretty.Simple (pTrace, pTraceM)
import Options.Applicative (ParseError (..), ReadM)
import Options.Applicative.Types (ReadM (..))

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
-- For example, converting 100 milliseconds to seconds should give us 0.1 seconds but with integer division, because of the rounding, we'd get 0 seconds
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

genericShow :: (IsString s, Show a) => a -> s
genericShow = fromString . show

-- | From https://www.stackage.org/haddock/lts-24.38/relude-1.2.2.2/src/Relude.Enum.html#inverseMap
inverseMap ::
    forall a k.
    (Bounded a, Enum a, Ord k) =>
    (a -> k) ->
    (k -> Maybe a)
inverseMap f = (`M.lookup` dict)
  where
    dict :: Map k a
    dict = M.fromList (fmapToFst f (universe @a))

-- | From https://www.stackage.org/haddock/lts-24.38/relude-1.2.2.2/src/Relude.Enum.html#universe
universe :: (Bounded a, Enum a) => [a]
universe = [minBound .. maxBound]

-- | From https://www.stackage.org/haddock/lts-24.38/relude-1.2.2.2/src/Relude.Extra.Tuple.html#fmapToFst
fmapToFst :: (Functor f) => (a -> b) -> f a -> f (b, a)
fmapToFst = fmap . toFst

-- | From https://www.stackage.org/haddock/lts-24.38/relude-1.2.2.2/src/Relude.Extra.Tuple.html#toFst
toFst :: (a -> b) -> a -> (b, a)
toFst f a = (f a, a)

-- | Based on https://www.stackage.org/haddock/lts-24.46/optparse-applicative-0.18.1.0/src/Options.Applicative.Internal.html#runReadM for pure execution of the optparse-applicative parsers outside of the optparse-applicative framework
runReadM :: forall b c. (IsString b) => ReadM c -> String -> Either b c
runReadM (ReadM r) s = mapLeft renderParseError $ runExcept $ runReaderT r s
  where
    renderParseError :: ParseError -> b
    renderParseError (ErrorMsg err) = fromString err
    renderParseError (InfoMsg msg) = fromString msg
    renderParseError UnknownError = "Unknown Error"
    renderParseError (UnexpectedError str _) = fromString $ "An error occurred: " <> str
    renderParseError (ExpectsArgError err) = fromString err
    renderParseError (MissingError _ _) = "Something is missing"
    renderParseError (ShowHelpText _) = "Error"
