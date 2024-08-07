module Utils where

import Debug.Trace (trace, traceShowM)

myTrace :: (Show a) => String -> a -> a
myTrace str' a = trace (str' <> show a) a

myTraceM :: (Show a, Applicative f) => String -> a -> f ()
myTraceM str a = traceShowM (str <> show a)
