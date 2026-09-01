{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | The types here should be used qualified
module Redis.Server.Config.Types (
    RedisConfigF (..),
    RedisConfig,
    PartialRedisConfig,
    collectFieldSpecs,
    gZipWith,
    collectNamedFields,
    NamedField (..),

    -- * Field Types
    RDBFileDir,
    RDBFilename,
    UseRDBCompression,
    GenRDBChecksum,
    RedisPort,

    -- * Field Type Accessors
    ConfigFieldType,
    getConfigFieldName,
) where

import Path

import Control.Applicative (Const (..))
import Control.Monad.Identity (Identity)
import Data.Data (Proxy (..))
import Data.Monoid (Last)
import Data.String (IsString (fromString))
import Data.Text (Text)
import GHC.Base (Symbol, Type)
import GHC.Generics (
    Generic (..),
    Generically (..),
    K1 (..),
    M1 (..),
    Selector (selName),
    (:*:) (..),
 )
import GHC.TypeLits (KnownSymbol, symbolVal)
import Redis.Utils (ShowBS (..))
import System.FilePath.Posix (dropTrailingPathSeparator)

data ConfigField = ConfigField Symbol Type

type family ConfigFieldName (k :: ConfigField) :: Symbol where
    ConfigFieldName ('ConfigField name _) = name

type family ConfigFieldType (k :: ConfigField) :: Type where
    ConfigFieldType ('ConfigField _ fieldType) = fieldType

-- -------------------------------------------------------------------------- --
--                         Config Field Types & Names                         --
-- -------------------------------------------------------------------------- --

type RDBFileDir = 'ConfigField "dir" RDBFileDirType
type RDBFileDirType = SomeBase Dir

instance ShowBS RDBFileDirType where
    showBs = fromString . dropTrailingPathSeparator . fromSomeDir

type RDBFilename = 'ConfigField "dbfilename" RDBFilenameType
type RDBFilenameType = Path Rel File

instance ShowBS RDBFilenameType where
    showBs = fromString . toFilePath

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

{-
    The purpose of this Generic typeclass is to extract the field values of a record. Specifically, like GZipWith it is coupled to our use case. The purpose of GFieldSpecs is to extract Const values out of a record. It works by recursively traversing the typical shape of the generic representation of a record -- something like (M1 (M1 (K1 _) :*: M1 (K1 _))) at the term level -- to extract and combine the field values of each record field into a list.

    The instances guide our recursive traversal: how to go from the top-level (M1 _) root to the (K1 _) leaf.

    We implemented this to have a type-safe way of filtering through lexer tokens gotten from parsing a raw redis config file. If we did things the usual way, then we'd have a case expression pattern matching on config fields we support. This is not type-safe because there's nothing to warn us if we misspell a field name or omit it. With this approach, any changes to RedisConfigF must be propagated to our config specs instantiation of RedisConfigF, so the type system helps us ensure that everything remains consistent.
-}
class GFieldSpecs a rep where
    gFieldSpecs :: rep p -> [a]

instance (GFieldSpecs a f) => GFieldSpecs a (M1 i c f) where
    gFieldSpecs (M1 x) = gFieldSpecs x

instance (GFieldSpecs a f, GFieldSpecs a g) => GFieldSpecs a (f :*: g) where
    gFieldSpecs (f :*: g) = gFieldSpecs f <> gFieldSpecs g

instance GFieldSpecs a (K1 i (Const a b)) where
    gFieldSpecs (K1 (Const x)) = [x]

{-
    This typeclass is for implementing a generic way to zip two records together using some function `fn`, like we do with lists using `zipWith` from Prelude. However, the zipper function in this case is a bit specialized to our use case of falling back to a default value of type `x` if `Last x` is `Nothing`. Before I got here, I tried a couple of things:

    First I tried putting `a`, `b`, and `c` in the head of the typeclass with the zipper fn being of type `(a -> b -> c)`. However, that didn't work because with gZipWith, we're dealing with records, which are often heterogeneous, not one set of type `a` and another set of type `b`.
    Thus, by including `a`, `b`, and `c` in the head of the typeclass, I added them to the "composite key" used to index/identify a GZipWith instance.
    I didn't write a GZipWith instance for every desired permutation of (`a`, `b`, `c`); this is because I didn't need to. I had a single parametrically polymorphic GZipWith instance, which meant the types for `a`, `b`, and `c` could vary, but the implementation remained the same. The compiler could then unify/"fill in" the type params with whatever types to get a fully qualified instance.

    The problem, well, problems — there are two:
        1. Whatever types the compiler resolves the type params (`a`, `b`, and `c`) to are fixed for the entirety of a given call site. So with gZipWith, once we reach the first set of record fields to zip, the compiler fixes the type params on our polymorphic instances to those record field types. This makes it so that no other instances with different instantiated type params may be leveraged at *that* call site.
        2. The zipper function type parameters also get fixed at the call site and can never change because gZipWith is a rank-1 function.

    Problem #1 is the reason why I do not want our GZipWith instances to be indexable using `a`, `b`, and `c`, so I opted to remove them from the class head.

    Next, I tried making the zipper function a rank-2 type with the signature `(forall a b c. a -> b -> c)`. This way the zipper function would remain polymorphic at the call site of gZipWith, but could be specialized to different types when being called on the actual heterogeneous record fields. This type checked, but the type is uninhabited (except by `undefined`) because `c` could be anything and is completely unrelated to `a` and `b`. At the call site for gZipWith we'd need an implementation that accounts for all potential instantiations of `c` (so any type) and that isn't possible since we have no `c` to work with at the term level. Constraints may help, but which ones? A useful one here would be something that allows us to create a value of type `c` out of nothing, the way `Monoid c` hands us `mempty` or `Bounded c` hands us `minBound`. None of those would be relevant for a zipper, though. Moreover, because it's a rank-2 signature, the call site of gZipWith cannot make any claims about, or pick, what types `a`, `b`, or `c` are. They must remain polymorphic/abstract there. It is for these reasons that I decided to do away with having `(forall a b c. a -> b -> c)` as the zipper's signature.

    From there, I massaged the zipper function's signature to align more closely with the signature of the zipper I wanted to use: `(forall x. x -> Last x -> x)`. Yes, it makes the entire thing less generally useful, but it type checks and works. We could also have parameterized on `Last`, that is, made it abstract, but since it never changes that gives us nothing, so I'm keeping it as `Last`.

    When all is said and done, we could have written this manually, but I wanted to take a stab at defining something like this using Generics.
-}

class GZipWith repA repB repC where
    gZipWith :: (forall x. x -> Last x -> x) -> repA a -> repB b -> repC c

instance (GZipWith f g h) => GZipWith (M1 i c f) (M1 i c g) (M1 i c h) where
    gZipWith fn (M1 x) (M1 y) = M1 (gZipWith fn x y)

instance (GZipWith f h l, GZipWith g i m) => GZipWith (f :*: g) (h :*: i) (l :*: m) where
    gZipWith fn (f :*: g) (h :*: i) = gZipWith fn f h :*: gZipWith fn g i

instance GZipWith (K1 i x) (K1 i (Last x)) (K1 i x) where
    gZipWith fn (K1 x) (K1 y) = K1 (fn x y)

-- We're defining an existential type because named fields are going to be of different types. Basically we'd need a heterogeneous list which can only achieved using existentials
data NamedField = forall a. (ShowBS a, Show a) => NamedField
    { name :: Text
    , val :: a
    }

deriving stock instance Show NamedField

{-
    Similar to GFieldSpecs, the purpose of GNamedFields is to traverse through the generic representation of a record and collect, in a list, field values with their corresponding field names.
-}
class GNamedFields rep where
    gNamedFields :: rep p -> [NamedField]

-- This instance is for traversing down into Metadata nodes that aren't parents to a leaf with record field value info (K1 node). Such nodes are usually ancestor nodes or the root node of a generic rep
instance (GNamedFields f) => GNamedFields (M1 i c f) where
    gNamedFields (M1 x) = gNamedFields x

instance (GNamedFields f, GNamedFields g) => GNamedFields (f :*: g) where
    gNamedFields (f :*: g) = gNamedFields f <> gNamedFields g

{-
    This instance is a more specific version of the one we have above. We've marked it as overlapping to tell the compiler to prefer it over the more generic M1 instance where applicable.

    Whilst inspecting the generic rep of our RedisConfigF type, I noticed that some M1 nodes in the tree contained the field name of a record field in their `Meta` param slot. These M1 nodes were also all direct parents to leaf K1 nodes that contained the record field type information. Therefore, to get both the field name and the value we needed to match on that specific sub-tree of M1 node directly followed by a K1 leaf so we could access both info at once.

    selName comes from the Selector type class to actually access the record field name
-}
instance {-# OVERLAPPING #-} (Selector c, ShowBS x, Show x) => GNamedFields (M1 i c (K1 r x)) where
    gNamedFields selectorMeta@(M1 (K1 x)) = [NamedField (fromString $ selName selectorMeta) x]

collectFieldSpecs :: (GFieldSpecs a (Rep (RedisConfigF f))) => RedisConfigF f -> [a]
collectFieldSpecs = gFieldSpecs . from

getConfigFieldName :: forall (b :: ConfigField) a. (KnownSymbol (ConfigFieldName b), IsString a) => a
getConfigFieldName = fromString $ symbolVal $ Proxy @(ConfigFieldName b)

collectNamedFields :: (GNamedFields (Rep (RedisConfigF f))) => RedisConfigF f -> [NamedField]
collectNamedFields = gNamedFields . from
