{- HLINT ignore "Redundant if" -}
-- TODO Remove this when slave support is implemented and the connectedSlaves field is populated correctly for both master and slave roles.

module Redis.Commands.Info.Replication (
    genReplicationInfoSectionStr,
) where

import Data.Text qualified as T
import Effectful.Concurrent.STM qualified as STMEff
import Effectful.Reader.Static qualified as ReaderEff

import Data.String (fromString)
import Data.Text (Text)
import Data.Word (Word8)
import Effectful (Eff)
import Optics (view)
import Redis.Commands.Info.Common (formatToInfoSectionField)
import Redis.Effects (RedisServerConfig)
import Redis.Server.Config (RedisConfigF (..), ReplicaOfType (NO_ONE))

data ReplicationInfo = ReplicationInfo
    { role :: ReplicationRole -- "master" or "slave"
    , connectedSlaves :: Word8 -- For master, the number of connected slaves. For slave, the number of connected slaves is always 0 since slaves can't have slaves connected to them
    , masterReplId :: Maybe Text -- For slave, the replication ID of its master. For master, this field is always Nothing since masters don't have a master above them
    , masterReplOffset :: Maybe Word -- For slave, the replication offset of its master. For master, this field is always Nothing since masters don't have a master above them
    }

data ReplicationRole = Master | Slave
    deriving stock (Eq, Show)

mkReplicationInfo ::
    forall r es. (RedisServerConfig r es) => Eff es ReplicationInfo
mkReplicationInfo =
    do
        env <- ReaderEff.ask @r

        let serverConfigRef = view #serverConfigRef env
        redisConfig <- STMEff.readTVarIO serverConfigRef

        let role = if redisConfig.replicaOf == NO_ONE then Master else Slave

        -- Always 0 if role is Slave
        let connectedSlaves = 0
        pure $
            ReplicationInfo
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

genReplicationInfoSectionStr :: forall r es. (RedisServerConfig r es) => Eff es Text
genReplicationInfoSectionStr = mkReplicationInfoText <$> mkReplicationInfo @r
