module Redis.Server.Config.Readers (
    rdbFileDirReader,
    rdbFilenameReader,
    rdbCompressionReader,
    rdbChecksumReader,
    portReader,
    replicaOfReader,
) where

import Path

import Data.Attoparsec.Text qualified as ATT
import Redis.Server.Config.Types qualified as Config

import Control.Applicative ((<|>))
import Data.Bits (toIntegralSized)
import Data.Char (isSpace)
import Data.String (fromString)
import Data.Word (Word16)
import Options.Applicative (ReadM, auto, eitherReader, maybeReader)
import Options.Applicative.Types (readerAsk)
import Redis.Helpers (withCustomError)
import Redis.Server.Config.Types (HostPortRec (HostPortRec))
import Redis.Utils (toLowerCaseString)
import Rerefined (refine)

rdbFileDirReader :: ReadM (Config.ConfigFieldType Config.RDBFileDir)
rdbFileDirReader = maybeReader (fmap Abs . parseAbsDir) <|> maybeReader (fmap Rel . parseRelDir)

rdbFilenameReader :: ReadM (Config.ConfigFieldType Config.RDBFilename)
rdbFilenameReader = maybeReader parseRelFile

rdbCompressionReader :: ReadM (Config.ConfigFieldType Config.UseRDBCompression)
rdbCompressionReader = modifiedBoolReader

rdbChecksumReader :: ReadM (Config.ConfigFieldType Config.GenRDBChecksum)
rdbChecksumReader = modifiedBoolReader

portReader :: ReadM (Config.ConfigFieldType Config.RedisPort)
portReader = eitherReader (ATT.parseOnly (portParser <* ATT.endOfInput) . fromString)

{- | Parses a TCP port number (1-65535).

Doesn't check for end of input, so it can be used inside larger parsers (see 'replicaOfReader').
-}
portParser :: ATT.Parser Config.Port
portParser = do
    -- Parse as an unbounded 'Integer' first: 'ATT.decimal' keyed to a fixed-width type wraps around instead of failing (e.g. "70000" parsed as a 'Word16' is 4464)
    rawPort <- withCustomError (ATT.decimal @Integer) "Expected a port number"
    let outOfRangeErr = "Expected a port between 1 and 65535 but got " <> show rawPort
    -- 'toIntegralSized' enforces the upper bound; the 'Config.Port' refinement enforces the lower one
    port16 <- maybe (fail outOfRangeErr) pure (toIntegralSized @Integer @Word16 rawPort)
    either (const $ fail outOfRangeErr) pure (refine port16)

{- | Reads a @replicaof@ value: either @NO ONE@ or @\<host\> \<port\>@.

The host is only required to be non-empty, since Redis accepts hostnames as well as IP addresses.
-}
replicaOfReader :: ReadM (Config.ConfigFieldType Config.ReplicaOf)
replicaOfReader = eitherReader (ATT.parseOnly replicaOfParser . fromString)
  where
    replicaOfParser = noOneParser <|> hostPortParser

    noOneParser = Config.NO_ONE <$ (ATT.asciiCI "NO ONE" <* ATT.endOfInput)

    hostPortParser =
        let hostParser = withCustomError (ATT.takeWhile1 (not . isSpace)) "Expected a hostname or IP address"
         in Config.HostPort <$> (HostPortRec <$> hostParser <*> (ATT.skipSpace *> portParser <* ATT.endOfInput))

modifiedBoolReader :: ReadM Bool
modifiedBoolReader = auto @Bool <|> yesNoReader
  where
    yesNoReader :: ReadM Bool
    yesNoReader = do
        rawBool <- toLowerCaseString <$> readerAsk
        case rawBool of
            "yes" -> pure True
            "no" -> pure False
            _ -> fail $ "Expected 'yes' or 'no' but got " <> rawBool
