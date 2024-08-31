module Commands.String.Get where

import Data.ByteString (ByteString)

newtype GetCmd = GetCmd {key :: ByteString} deriving (Eq, Show)
