{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Redis.Server.Context where

import Control.Lens (camelCaseFields, makeLensesWith)
import Network.Socket (Socket)
import Redis.Server.Settings (ServerSettings)
import Redis.Store (StoreState)

data ServerContext = ServerContext
    { serverContextClientSocket :: Socket
    , serverContextStore :: StoreState
    , serverContextSettings :: ServerSettings
    }

makeLensesWith camelCaseFields ''ServerContext
