{- HLINT ignore "Redundant if" -}
-- TODO Remove this when slave support is implemented and the connectedSlaves field is populated correctly for both master and slave roles.

module Redis.Commands.Info.Replication (
    genReplicationInfoSectionStr,
) where

import Data.Text qualified as T

import Data.String (fromString)
import Data.Text (Text)
import Data.Word (Word8)
import Redis.Commands.Info.Common (formatToInfoSectionField)

data ReplicationInfo = ReplicationInfo
    { role :: ReplicationRole -- "master" or "slave"
    , connectedSlaves :: Word8 -- For master, the number of connected slaves. For slave, the number of connected slaves is always 0 since slaves can't have slaves connected to them
    , masterReplId :: Maybe Text -- For slave, the replication ID of its master. For master, this field is always Nothing since masters don't have a master above them
    , masterReplOffset :: Maybe Word -- For slave, the replication offset of its master. For master, this field is always Nothing since masters don't have a master above them
    }

data ReplicationRole = Master | Slave
    deriving stock (Eq, Show)

mkReplicationInfo :: ReplicationInfo
mkReplicationInfo =
    let role = Master
        connectedSlaves = if role == Slave then 0 else 0 -- TODO: Implement slave support and populate this field correctly for both master and slave roles.
     in ReplicationInfo
            { role = role
            , connectedSlaves = connectedSlaves
            , masterReplId = Nothing
            , masterReplOffset = Nothing
            }

mkReplicationInfoText :: ReplicationInfo -> Text
mkReplicationInfoText info =
    "# Replication\r\n"
        <> mconcat
            [ formatToInfoSectionField "role" (T.toLower . fromString . show $ info.role)
            , formatToInfoSectionField "connected_slaves" (fromString . show $ info.connectedSlaves)
            , formatToInfoSectionField "master_replid" (T.toLower . fromString . show $ info.masterReplId)
            , formatToInfoSectionField "master_repl_offset" (T.toLower . fromString . show $ info.masterReplOffset)
            ]

genReplicationInfoSectionStr :: Text
genReplicationInfoSectionStr = mkReplicationInfoText mkReplicationInfo
