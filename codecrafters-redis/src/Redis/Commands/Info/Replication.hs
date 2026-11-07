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
import Redis.Commands.Info.Common (formatToInfoSectionField, maskFieldWhenNecessary)
import Redis.Effects (RedisServerConfig, RedisServerMetadata)
import Redis.Server.Config (RedisConfigF (..), ReplicaOfType (NO_ONE))
import Redis.Server.Metadata (
    Environment,
    ReplId,
    ServerMetadata (..),
    replIdToText,
 )

data ReplicationInfo = ReplicationInfo
    { role :: ReplicationRole
    , connectedSlaves :: Word8
    -- ^ For master, the number of connected slaves. For slave, the number of connected slaves is often 0, but redis does support slaves having slaves (chained replication)
    , masterReplId :: ReplId
    -- ^ master replid is short for master replication Id. It is also just called a "replid". It is a pseudo random hexadecimal string (all lowercase) of 40 characters. It is generated once per server startup. All redis servers have a replid. Masters get to keep theirs, for the most part, while slaves take on the replid of their masters upon the completion of a full sync. A master and slaves replid changes whenever a new replication history is started.
    , masterReplOffset :: Word
    -- ^ This field has two meanings. On a master it represents how many bytes of replication stream the master has written in the current replication log. For a slave, this value refers to the number of bytes of the master's replication history that the slave has consumed/processed
    }

data ReplicationRole = Master | Slave
    deriving stock (Eq, Show)

mkReplicationInfo ::
    forall r es. (RedisServerConfig r es, RedisServerMetadata r es) => Eff es ReplicationInfo
mkReplicationInfo =
    do
        env <- ReaderEff.ask @r

        let serverConfigRef = view #serverConfigRef env
        let serverMetadata = view #serverMetadata env

        {-
            Read both in one transaction so INFO shows a consistent view. `REPLICAOF` will be able to change the role and start a new replication history, so two separate reads could pair the new role with the old replid.
        -}
        (redisConfig, replId) <- STMEff.atomically $ (,) <$> STMEff.readTVar serverConfigRef <*> STMEff.readTVar serverMetadata.replIdRef

        let role = if redisConfig.replicaOf == NO_ONE then Master else Slave

        let connectedSlaves = 0
        pure $
            ReplicationInfo
                { role = role
                , connectedSlaves = connectedSlaves
                , masterReplId = replId
                , masterReplOffset = 0
                }

mkReplicationInfoText :: Environment -> ReplicationInfo -> Text
mkReplicationInfoText serverEnv info =
    let maskFieldDuringTesting = maskFieldWhenNecessary serverEnv
     in "# Replication\r\n"
            <> mconcat
                [ formatToInfoSectionField "role" (T.toLower . fromString . show $ info.role)
                , formatToInfoSectionField "connected_slaves" (fromString . show $ info.connectedSlaves)
                , formatToInfoSectionField "master_replid" . maskFieldDuringTesting . replIdToText $ info.masterReplId
                , formatToInfoSectionField "master_repl_offset" (T.toLower . fromString . show $ info.masterReplOffset)
                ]

genReplicationInfoSectionStr :: forall r es. (RedisServerConfig r es, RedisServerMetadata r es) => Environment -> Eff es Text
genReplicationInfoSectionStr serverEnv = mkReplicationInfoText serverEnv <$> mkReplicationInfo @r
