module Redis.Server.Config.Readers where

import Path

import Redis.Server.Config.Types qualified as Config

import Control.Applicative ((<|>))
import Options.Applicative (ReadM, auto, maybeReader)

rdbFileDirReader :: ReadM (Config.ConfigFieldType Config.RDBFileDir)
rdbFileDirReader = maybeReader (fmap Abs . parseAbsDir) <|> maybeReader (fmap Rel . parseRelDir)

rdbFilenameReader :: ReadM (Config.ConfigFieldType Config.RDBFilename)
rdbFilenameReader = maybeReader parseRelFile

rdbCompressionReader :: ReadM (Config.ConfigFieldType Config.UseRDBCompression)
rdbCompressionReader = auto

rdbChecksumReader :: ReadM (Config.ConfigFieldType Config.GenRDBChecksum)
rdbChecksumReader = auto

portReader :: ReadM (Config.ConfigFieldType Config.RedisPort)
portReader = auto
