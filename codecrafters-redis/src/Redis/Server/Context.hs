{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Redis.Server.Context where

import GHC.Generics (Generic)
import Network.Socket (Socket)
import Optics (makeFieldLabelsNoPrefix)
import Redis.Server.Settings (ServerSettings)
import Redis.Store (StoreState)

data ServerContext = ServerContext
    { clientSocket :: Socket
    , store :: StoreState
    , settings :: ServerSettings
    }
    deriving stock (Eq, Generic)

makeFieldLabelsNoPrefix ''ServerContext
