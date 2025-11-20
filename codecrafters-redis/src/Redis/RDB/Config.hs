{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- TODO: Export only the smart constructor for RDBConfig

module Redis.RDB.Config where

import GHC.Generics (Generic)
import Optics (makeFieldLabelsNoPrefix)

data RDBConfig = RDBConfig
    { useLzfCompression :: Bool
    , generateChecksum :: Bool
    , skipChecksumValidation :: Bool
    }
    deriving stock (Eq, Show, Generic)

makeFieldLabelsNoPrefix ''RDBConfig

defaultRDBConfig :: RDBConfig
defaultRDBConfig =
    RDBConfig
        { useLzfCompression = False
        , generateChecksum = True
        , skipChecksumValidation = False
        }

data MkRDBConfigArg = MkRDBConfigArg
    { useCompression :: Bool
    , generateChecksum :: Bool
    }
    deriving stock (Eq, Show, Generic)

mkRDBConfig :: MkRDBConfigArg -> RDBConfig
mkRDBConfig MkRDBConfigArg{..} =
    RDBConfig
        { useLzfCompression = useCompression
        , generateChecksum = generateChecksum
        , skipChecksumValidation = not generateChecksum
        }
