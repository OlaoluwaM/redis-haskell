{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Server.Env where

import Control.Lens (camelCaseFields, makeLensesWith)
import Network.Socket (Socket)

data Env = Env
    { envClientSocket :: Socket
    , envStore :: ()
    }

makeLensesWith camelCaseFields ''Env
