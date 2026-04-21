{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Redis.Server.Context where

import GHC.Generics (Generic)
import Network.Socket (Socket)
import Optics (makeFieldLabelsNoPrefix)
import Redis.Server.Settings (ServerSettingsRef)
import Redis.ServerState (ServerState)

-- Having both the store and setting fields be shared mutable variables make it such that threads can access and modify them in parallel without fear of conflicts and retries
data ServerContext = ServerContext
    { clientSocket :: Socket -- readonly, no need to make mutable
    , serverState :: ServerState -- The`ServerState` fields are all already shared mutable variables so I don't think we'd gain anything by wrapping in it again in a shared mutable variable. If anything, we gain greater concurrent access granularity this way. Threads may modify fields independently without blocking
    , serverSettingsRef :: ServerSettingsRef -- Making this a shared mutable variable makes sense because it allows for multi-threaded access without conflicting with those threads who only care about the server state. More importantly, if this `settings` field were not a shared mutable variable, changes to it won't propagate to other threads since the ServerContext itself is passed around to threads as a regular argument, and not as a shared mutable variable. Wait, but isn't this always going to be readonly? Perhaps there's no reason to make it mutable. Actually, there is since Redis has a command to allow for modifying settings at run time: CONFIG SET
    }
    deriving stock (Generic)

makeFieldLabelsNoPrefix ''ServerContext
