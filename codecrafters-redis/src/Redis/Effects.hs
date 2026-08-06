module Redis.Effects (
    HasClientSocket,
    RedisClientCommunication,
    HasServerState,
    RedisServerState,
    HasServerConfig,
    HasServerMetadata,
    RedisServerConfig,
    RedisServerMetadata,
    RDBWrite,
    ServerEffects,
    Logging,
) where

import Effectful (IOE, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.FileSystem (FileSystem)
import Effectful.Reader.Static (Reader)
import Network.Socket (Socket)
import Optics (A_Lens, LabelOptic)
import Redis.Effect.Communication (Communication)
import Redis.Effect.Logging (Logging)
import Redis.Effect.Time (Time)
import Redis.Server.Context (ServerConfigRef)
import Redis.Server.Metadata (ServerMetadata)
import Redis.ServerState (ServerState)

type HasClientSocket r = (LabelOptic "clientSocket" A_Lens r r Socket Socket)
type HasServerState r = (LabelOptic "serverState" A_Lens r r ServerState ServerState)

type HasServerConfig r = (LabelOptic "serverConfigRef" A_Lens r r ServerConfigRef ServerConfigRef)
type HasServerMetadata r = (LabelOptic "serverMetadata" A_Lens r r ServerMetadata ServerMetadata)

type RedisClientCommunication r es =
    ( HasClientSocket r
    , Reader r :> es
    , Communication :> es
    )

type RedisServerState r es =
    ( HasServerState r
    , Reader r :> es
    , Concurrent :> es
    , Time :> es
    )

type RedisServerConfig r es =
    ( HasServerConfig r
    , Reader r :> es
    , Concurrent :> es
    )

type RedisServerMetadata r es =
    ( HasServerMetadata r
    , Reader r :> es
    )

type RDBWrite r es =
    ( RedisClientCommunication r es
    , RedisServerState r es
    , RedisServerConfig r es
    , Logging :> es
    , FileSystem :> es
    )

type ServerEffects r =
    '[ Reader r
     , FileSystem
     , Concurrent
     , Time
     , Logging
     , Communication
     , IOE
     ]
