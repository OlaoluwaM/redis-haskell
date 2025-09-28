{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Redis.RDB.Config where

import GHC.Generics (Generic)
import Optics (makeFieldLabelsNoPrefix)

data RDBConfig = RDBConfig
    { useLzfCompression :: Bool
    , generateChecksum :: Bool
    , skipChecksumValidation :: Bool
    }
    deriving stock (Eq, Show, Generic)

defaultRDBConfig :: RDBConfig
defaultRDBConfig =
    RDBConfig
        { useLzfCompression = False
        , generateChecksum = True
        , skipChecksumValidation = False
        }

makeFieldLabelsNoPrefix ''RDBConfig
