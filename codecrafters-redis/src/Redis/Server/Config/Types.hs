{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

-- {-# OPTIONS_GHC -Wno-missing-methods #-}

-- | The types here should be used qualified
module Redis.Server.Config.Types (
    RedisConfigF (..),
    RedisConfig,
    PartialRedisConfig,

    -- * Field Types
    RDBFileDir,
    RDBFilename,
    UseRDBCompression,
    GenRDBChecksum,
    RedisPort,

    -- * Field Type Accessors
    ConfigFieldType,
    collectFieldSpecs,
    getConfigFieldName,
) where

import Path

import Control.Applicative (Const (..))
import Control.Monad.Identity (Identity)
import Data.Data (Proxy (..))
import Data.Monoid (Last)
import Data.String (IsString (fromString))
import GHC.Base (Symbol, Type)
import GHC.Generics (
    Generic (..),
    Generically (..),
    K1 (..),
    M1 (..),
    (:*:) (..),
 )
import GHC.TypeLits (KnownSymbol, symbolVal)

-- TODO: Before we carry one, refine the notes we've made in our Obsidian vault on HKD and Data kinds with regards to this project

data ConfigField = ConfigField Symbol Type

type family ConfigFieldName (k :: ConfigField) :: Symbol where
    ConfigFieldName ('ConfigField name _) = name

type family ConfigFieldType (k :: ConfigField) :: Type where
    ConfigFieldType ('ConfigField _ fieldType) = fieldType

-- -------------------------------------------------------------------------- --
--                         Config Field Types & Names                         --
-- -------------------------------------------------------------------------- --

type RDBFileDir = 'ConfigField "dir" (SomeBase Dir)

type RDBFilename = 'ConfigField "dbfilename" (Path Rel File)

type UseRDBCompression = 'ConfigField "rdbcompression" Bool

type GenRDBChecksum = 'ConfigField "rdbchecksum" Bool

type RedisPort = 'ConfigField "port" Int

-- -------------------------------------------------------------------------- --

type family HKD (f :: Type -> Type) (a :: ConfigField) where
    HKD Identity a = (ConfigFieldType a) -- Not necessary, but helps with automatic unwrapping of Identity a
    HKD f a = f (ConfigFieldType a)

data RedisConfigF (f :: Type -> Type) = RedisConfigF
    { rdbFileDirPath :: HKD f RDBFileDir
    , rdbFilenamePath :: HKD f RDBFilename
    , useRDBCompression :: HKD f UseRDBCompression
    , genRdbChecksum :: HKD f GenRDBChecksum
    , port :: HKD f RedisPort
    }
    deriving stock (Generic)

type RedisConfig = RedisConfigF Identity
type PartialRedisConfig = RedisConfigF Last

deriving stock instance
    ( Show (HKD f RDBFileDir)
    , Show (HKD f RDBFilename)
    , Show (HKD f UseRDBCompression)
    , Show (HKD f GenRDBChecksum)
    , Show (HKD f RedisPort)
    ) =>
    Show (RedisConfigF f)

deriving stock instance
    ( Eq (HKD f RDBFileDir)
    , Eq (HKD f RDBFilename)
    , Eq (HKD f UseRDBCompression)
    , Eq (HKD f GenRDBChecksum)
    , Eq (HKD f RedisPort)
    ) =>
    Eq (RedisConfigF f)

-- We technically don't need this, but it is interesting to see how one can derive semigroup for record type without having to manually (<>) each field. We only really need this for the PartialRedisConfig instance
deriving via
    Generically (RedisConfigF f)
    instance
        ( Semigroup (HKD f RDBFileDir)
        , Semigroup (HKD f RDBFilename)
        , Semigroup (HKD f UseRDBCompression)
        , Semigroup (HKD f GenRDBChecksum)
        , Semigroup (HKD f RedisPort)
        ) =>
        Semigroup (RedisConfigF f)

deriving via
    Generically (RedisConfigF f)
    instance
        ( Monoid (HKD f RDBFileDir)
        , Monoid (HKD f RDBFilename)
        , Monoid (HKD f UseRDBCompression)
        , Monoid (HKD f GenRDBChecksum)
        , Monoid (HKD f RedisPort)
        ) =>
        Monoid (RedisConfigF f)

class GFieldSpecs a rep where
    gFieldSpecs :: rep p -> [a]

-- With this instance, we can traverse past the M1s
instance (GFieldSpecs a f) => GFieldSpecs a (M1 i c f) where
    gFieldSpecs (M1 x) = gFieldSpecs x

instance (GFieldSpecs a f, GFieldSpecs a g) => GFieldSpecs a (f :*: g) where
    gFieldSpecs (f :*: g) = gFieldSpecs f <> gFieldSpecs g

instance GFieldSpecs a (K1 i (Const a b)) where
    gFieldSpecs (K1 (Const x)) = [x]

collectFieldSpecs :: (Generic (RedisConfigF f), GFieldSpecs a (Rep (RedisConfigF f))) => RedisConfigF f -> [a]
collectFieldSpecs = gFieldSpecs . from

getConfigFieldName :: forall (b :: ConfigField) a. (KnownSymbol (ConfigFieldName b), IsString a) => a
getConfigFieldName = fromString $ symbolVal $ Proxy @(ConfigFieldName b)
