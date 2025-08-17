module Redis.RDB.Format (
    KeyValueOpCode (..),
    EOF (..),
    KeyValWithExpiryInMS (..),
    KeyValWithExpiryInS (..),
    KeyValWithNoExpiryInfo (..),
    AuxField (..),
    ValueType (..),
    RDBFile (..),
    RDBMagicString (..),
    RDBVersion (..),
    RDbEntry (..),
    SelectDB (..),
    ResizeDB (..),
    RedisVersion (..),
    RedisBits (..),
    CTime (..),
    UsedMem (..),
    RDBFileWithChecksum (..),

    -- ** For Testing
    defaultChecksum,
) where

-- Core RDB data types and encoding functions
import Redis.RDB.Data
import Redis.RDB.SBinary

import Control.Monad.State.Strict qualified as State
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC

import Control.Applicative (asum, optional, (<|>))
import Control.Applicative.Combinators (many, manyTill)
import Control.Monad.Trans (lift)
import Data.Binary (Binary (..))
import Data.Binary.Get (
    getByteString,
    getWord64le,
 )
import Redis.RDB.SBinary.DerivingVia (BinaryFromSBinary (..))

import Data.Binary.Put (
    putByteString,
    putWord64le,
 )
import Data.Default (Default (..))
import Data.Word (Word8)
import Redis.RDB.CRC64 (CheckSum, fromChecksum, toChecksum)

{- | Redis RDB Format Implementation

This module provides a complete implementation of the Redis RDB (Redis Database)
binary file format used for database persistence and snapshots.

The RDB format is Redis's primary method for creating point-in-time snapshots
of the entire database state, enabling backup, replication, and restart functionality.

Core capabilities:
- Compact variable-length encoding to minimize file size
- Support for all Redis data types (strings, lists, sets, hashes, sorted sets)
- Precise expiry tracking (second or millisecond granularity)
- Rich metadata including Redis version, architecture, and creation time
- Data integrity protection via CRC64 checksums
- Multi-database support (Redis databases 0-15)

Binary File Structure:
The RDB file follows a strictly defined binary layout:

1. File Header:
   - Magic signature: "REDIS" (5 ASCII bytes)
   - Format version: 4-digit ASCII string (e.g., "0003", "0009")

2. Global Metadata (Auxiliary Fields):
   - Redis version that created the file
   - Server architecture (32-bit vs 64-bit)
   - File creation timestamp
   - Memory usage at save time

3. Database Sections (one per Redis database):
   - Database selector (specifies which DB: 0-15)
   - Size hints for memory pre-allocation
   - Key-value entries with optional expiry times

4. File Termination:
   - EOF marker (single 0xFF byte)
   - Optional CRC64 checksum for integrity verification

Space-Efficient Variable Length Encoding:
Redis employs a sophisticated encoding scheme to minimize storage overhead.
String lengths and integer values use a compact representation where the first
2 bits of the length byte determine the encoding strategy:

- 00xxxxxx: Short strings (0-63 bytes) - length in remaining 6 bits
- 01xxxxxx xxxxxxxx: Medium strings (0-16383 bytes) - length in 14 bits
- 10xxxxxx [4 bytes]: Large strings (up to 4GB) - length in next 32 bits
- 11xxxxxx: Special encodings for integers stored as strings

This approach can reduce file size by 20-40% compared to fixed-width encoding.

Control Opcodes:
The RDB format uses single-byte opcodes to mark different section types:

- 0xFF: END_OF_FILE - Signals the end of RDB data
- 0xFE: SELECT_DB - Switches to a different Redis database (0-15)
- 0xFD: EXPIRE_TIME - Key expires at specific Unix timestamp (seconds)
- 0xFC: EXPIRE_TIME_MS - Key expires at specific timestamp (milliseconds)
- 0xFB: RESIZE_DB - Memory allocation hints for hash table sizing
- 0xFA: AUXILIARY - Metadata fields (version, architecture, timestamps)

These opcodes allow parsers to efficiently navigate the file structure and
handle different data sections appropriately.
-}

{- | Complete Redis RDB file representation.

This is the top-level structure that represents an entire RDB file in memory.
The structure mirrors the binary file format exactly:

- File identification through magic string and version
- Global metadata via auxiliary fields
- Database contents organized by database index
- Optional integrity checking via CRC64 checksum

Most Redis deployments use only database 0, but the format supports up to 16
databases (0-15) to enable logical data separation within a single Redis instance.
Each database is completely independent with its own keyspace.
-}
data RDBFile = RDBFile
    { magicString :: RDBMagicString
    -- ^ File format identifier - must be exactly "REDIS"
    , version :: RDBVersion
    -- ^ RDB format version (e.g., "0003", "0009") determining encoding rules
    , auxFieldEntries :: [AuxField]
    -- ^ Global metadata: Redis version, architecture, creation time, memory stats
    , dbEntries :: [RDbEntry]
    -- ^ Database contents - typically just database 0, but can include 0-15
    }
    deriving stock (Show, Eq)

newtype RDBFileWithChecksum = RDBFileWithChecksum
    { getRDBFileWithChecksum :: RDBFile
    }
    deriving stock (Show, Eq)
    deriving (Binary) via (BinaryFromSBinary RDBFileWithChecksum)

{- | Individual Redis database within an RDB file.

Redis supports multiple logical databases (0-15) within a single instance,
though most applications use only database 0. Each database has:

- A unique identifier (database index)
- Memory optimization hints for efficient loading
- A collection of key-value pairs with optional expiry times

The resize hints help Redis pre-allocate appropriate hash table sizes during
loading, improving performance especially for large databases. These hints
are estimates and the actual number of keys may differ.
-}
data RDbEntry = RDbEntry
    { entryId :: SelectDB
    -- ^ Database identifier (0-15) - which logical Redis database these keys belong to
    , resizeDBEntry :: ResizeDB
    -- ^ Performance hints for memory allocation during database loading
    , keyValEntries :: [KeyValueOpCode]
    -- ^ All key-value pairs stored in this database, with optional expiry data
    }
    deriving stock (Show, Eq)
    deriving (Binary) via (BinaryFromSBinary RDbEntry)

{- | File format identifier for Redis RDB files.

Every valid RDB file must begin with exactly these 5 ASCII bytes: "REDIS".
This magic string serves as:
- File format detection (distinguishes RDB from other file types)
- Quick validation during file opening
- Protection against accidentally processing non-RDB files

If these bytes are missing or incorrect, the file is not a valid RDB file.
-}
data RDBMagicString = Redis
    deriving stock (Show, Eq)
    deriving (Binary) via (BinaryFromSBinary RDBMagicString)

{- | RDB format version identifier.

The version is stored as exactly 4 ASCII digits (e.g., "0003", "0009") and
determines which encoding schemes, opcodes, and data layouts are valid.
Higher version numbers may introduce new features while maintaining backward
compatibility for reading older files.

Version evolution:
- "0001": Redis 1.0 (basic string support)
- "0002": Redis 1.2 (added list and set types)
- "0003": Redis 2.0+ (hash support, improved encoding)
- "0006": Redis 2.6+ (sorted sets, expires)
- "0007": Redis 3.2+ (quicklist, geospatial)
- "0009": Redis 5.0+ (streams, modules)

Each version builds upon previous ones, adding new capabilities while
ensuring older RDB files remain readable.
-}
newtype RDBVersion = RDBVersion
    { getRDBVersion :: BS.ByteString
    }
    deriving stock (Show, Eq)
    deriving (Binary) via (BinaryFromSBinary RDBVersion)

{- | Key-value entry types with different expiry behaviors.

Redis stores three fundamentally different types of key-value entries:

1. Keys with millisecond-precision expiry (0xFC opcode)
   - Used for short-lived keys requiring precise timing
   - Timestamp stored as 64-bit milliseconds since Unix epoch

2. Keys with second-precision expiry (0xFD opcode)
   - Used for longer-lived keys where exact timing isn't critical
   - Timestamp stored as 32-bit seconds since Unix epoch (more compact)

3. Keys without expiry (no opcode prefix)
   - Persistent keys that live until explicitly deleted
   - Most common type in typical Redis usage

The choice between millisecond and second precision depends on the application's
timing requirements and storage efficiency considerations.
-}
data KeyValueOpCode = FCOpCode KeyValWithExpiryInMS | KeyValOpCode KeyValWithNoExpiryInfo | FDOpcode KeyValWithExpiryInS
    deriving stock (Show, Eq)
    deriving (Binary) via (BinaryFromSBinary KeyValueOpCode)

{- | File termination marker.

The EOF marker (single 0xFF byte) serves multiple critical purposes:
- Definitively marks the end of RDB data content
- Allows parsers to detect truncated or corrupted files
- Enables validation that the entire file was written successfully
- Separates data content from optional trailing checksum

After the EOF marker, some RDB versions append an 8-byte CRC64 checksum.
Any data appearing before a proper EOF marker indicates file corruption.
-}
data EOF = EOF
    deriving stock (Show, Eq)
    deriving (Binary) via (BinaryFromSBinary EOF)

{- | Key-value pair with millisecond-precision expiry.

This entry type is used when applications need precise control over key
lifetimes, such as:
- Session tokens with exact expiry times
- Rate limiting with sub-second windows
- Real-time cache entries
- Time-sensitive security credentials

Storage format: [0xFC][8-byte timestamp][value type][key][value]

The 64-bit timestamp provides millisecond precision from Unix epoch, allowing
expiry times up to year 292,277,026,596. Redis automatically removes expired
keys during normal operations or explicit cleanup.

Trade-off: Uses 4 extra bytes compared to second precision, but enables
precise timing for applications that need it.
-}
data KeyValWithExpiryInMS = KeyValWithExpiryInMS
    { expiryTimeMs :: RDBUnixTimestampMS
    -- ^ Expiry timestamp (stored internally as seconds, converted to/from milliseconds during serialization)
    , valueType :: ValueType
    -- ^ Redis data type identifier (string, list, set, hash, etc.)
    , encodedKey :: RDBLengthPrefixedVal
    -- ^ Key name using Redis variable-length string encoding
    , encodedValue :: RDBLengthPrefixedVal
    -- ^ Value data encoded according to its specific type rules
    }
    deriving stock (Show, Eq)
    deriving (Binary) via (BinaryFromSBinary KeyValWithExpiryInMS)

{- | Key-value pair with second-precision expiry.

This is the standard expiry format for most Redis use cases where exact
sub-second timing isn't required:
- Cache entries with minute/hour lifetimes
- User sessions
- Temporary data with longer durations
- Application-level TTL management

Storage format: [0xFD][4-byte timestamp][value type][key][value]

The 32-bit timestamp covers expiry times until year 2038 (Unix epoch limit).
This format saves 4 bytes per entry compared to millisecond precision,
providing meaningful storage savings for large databases.

Most Redis deployments use this format as it balances precision with efficiency.
-}
data KeyValWithExpiryInS = KeyValWithExpiryInS
    { expiryTimeS :: RDBUnixTimestampS
    -- ^ Expiry timestamp in Unix seconds (matches POSIXTime's native resolution)
    , valueType :: ValueType
    -- ^ Redis data type identifier (string, list, set, hash, etc.)
    , encodedKey :: RDBLengthPrefixedVal
    -- ^ Key name using Redis variable-length string encoding
    , encodedValue :: RDBLengthPrefixedVal
    -- ^ Value data encoded according to its specific type rules
    }
    deriving stock (Show, Eq)
    deriving (Binary) via (BinaryFromSBinary KeyValWithExpiryInS)

{- | Persistent key-value pair (no expiry).

This represents the most common type of Redis data - keys that persist
indefinitely until explicitly deleted by application code or admin commands.

Examples include:
- Application configuration settings
- User profile data
- Permanent cache entries
- Reference data and lookup tables

Storage format: [value type][key][value]

No opcode prefix is used since persistence is the default behavior.
This saves 1 byte per entry compared to expiry-enabled keys, and these
savings add up significantly in large databases.
-}
data KeyValWithNoExpiryInfo = KeyValWithNoExpiryInfo
    { valueType :: ValueType
    -- ^ Redis data type identifier (string, list, set, hash, etc.)
    , encodedKey :: RDBLengthPrefixedVal
    -- ^ Key name using Redis variable-length string encoding
    , encodedValue :: RDBLengthPrefixedVal
    -- ^ Value data encoded according to its specific type rules
    }
    deriving stock (Show, Eq)
    deriving (Binary) via (BinaryFromSBinary KeyValWithNoExpiryInfo)

{- | Database selection marker (opcode 0xFE).

Redis supports up to 16 independent logical databases (numbered 0-15) within
a single instance. This marker indicates which database the following key-value
entries belong to.

Database usage patterns:
- Database 0: Default database used by most applications
- Databases 1-15: Often used for testing, staging, or logical data separation
- Many Redis deployments use only database 0 for simplicity

The database selection remains active until another SelectDB marker is
encountered or the file ends.
-}
newtype SelectDB
    = -- | Database index (0-15) identifying which logical Redis database contains the following keys
      SelectDB {dbIndex :: Word8}
    deriving stock (Show, Eq)
    deriving (Binary) via (BinaryFromSBinary SelectDB)

instance Default SelectDB where
    def = SelectDB 0 -- Default to database 0

{- | Memory allocation optimization hints (opcode 0xFB).

These hints help Redis pre-allocate appropriately sized hash tables during
RDB loading, improving performance and reducing memory fragmentation.

The hints are estimates based on the database state when the RDB was created:
- Total key count helps size the main hash table
- Expiry key count helps size the expiry tracking structures

These are optimization hints only - the actual counts may differ and Redis
will handle any discrepancies gracefully. Providing accurate hints can
improve loading performance by 10-30% for large databases.
-}
data ResizeDB = ResizeDB
    { numOfKeys :: Word8
    -- ^ Estimated total number of keys in this database (for hash table sizing)
    , numOfKeysWithExpiry :: Word8
    -- ^ Estimated number of keys that have expiry times (for expiry tracking structures)
    }
    deriving stock (Show, Eq)

instance Default ResizeDB where
    def = ResizeDB 0 0

{- | Auxiliary field (opcode 0xFA) containing metadata.

These fields store metadata about the Redis instance and RDB file creation:
- Redis version that created the file (e.g., "7.0.0")
- Architecture information (32-bit or 64-bit)
- File creation timestamp (Unix time)
- Memory usage statistics at save time

Format: [0xFA][key-string][value-string]
Both key and value are encoded using RDB variable-length string encoding.

Common auxiliary fields:
- "redis-ver": Redis version (e.g., "7.0.0")
- "redis-bits": Architecture ("32" or "64")
- "ctime": Creation timestamp
- "used-mem": Memory usage in bytes
-}
data AuxField = AuxFieldRedisVer RedisVersion | AuxFieldRedisBits RedisBits | AuxFieldCTime CTime | AuxFieldUsedMem UsedMem | AuxFieldCustom RDBLengthPrefixedString RDBLengthPrefixedVal
    deriving stock (Show, Eq)
    deriving (Binary) via (BinaryFromSBinary AuxField)

-- | Memory usage information in bytes at the time of RDB creation
newtype UsedMem = UsedMem Int
    deriving stock (Show, Eq)
    deriving (Binary) via (BinaryFromSBinary UsedMem)

-- | RDB file creation timestamp as Unix time
newtype CTime = CTime RDBUnixTimestampS
    deriving stock (Show, Eq)
    deriving (Binary) via (BinaryFromSBinary CTime)

-- | Redis version string that created this RDB file
newtype RedisVersion = RedisVersion RDBLengthPrefixedShortString
    deriving stock (Show, Eq)
    deriving (Binary) via (BinaryFromSBinary RedisVersion)

-- | Architecture information (32-bit or 64-bit Redis build)
data RedisBits = RedisBits32 | RedisBits64
    deriving stock (Show, Eq)
    deriving (Binary) via (BinaryFromSBinary RedisBits)

{- | Redis value types supported in RDB format.

Each value type has a specific encoding scheme:
- String (0): Simple byte sequences, may use special integer encoding
- List (1): Sequence of string elements
- Set (2): Unordered collection of unique strings
- Sorted Set (3): Ordered collection with scores
- Hash (4): Key-value pairs (like a nested dictionary)
- Zipmap (9): Compact hash representation for small hashes
- Ziplist (10-14): Compact representations for small collections

Currently only string type is implemented.
-}
data ValueType = Str
    deriving stock (Show, Eq, Ord)
    deriving (Binary) via (BinaryFromSBinary ValueType)

{- | RDB format opcode constants
These single-byte values mark different section types in the RDB file format
-}
eofOpCodeByteTag :: Word8
eofOpCodeByteTag = 0xFF

selectDBOpCodeByteTag :: Word8
selectDBOpCodeByteTag = 0xFE

keyValWithExpiryInSpCodeByteTag :: Word8
keyValWithExpiryInSpCodeByteTag = 0xFD

keyValWithExpiryInMSOpCodeByteTag :: Word8
keyValWithExpiryInMSOpCodeByteTag = 0xFC

resizeDBOpCodeByteTag :: Word8
resizeDBOpCodeByteTag = 0xFB

auxiliaryFieldOpCodeByteTag :: Word8
auxiliaryFieldOpCodeByteTag = 0xFA

stringValueTypeByteId :: Word8
stringValueTypeByteId = 0

-- | Binary instances for RDB format serialization and deserialization

{- | Complete RDB file encoding/decoding.
Handles the full file structure including magic string, version, auxiliary fields,
database entries, EOF marker, and optional checksum.
-}
{-# WARNING in "x-unsafe-internals" defaultChecksum "This value is exported for testing purposes only" #-}
defaultChecksum :: CheckSum
defaultChecksum = toChecksum 0

instance Binary RDBFile where
    put (RDBFile magicString version auxFieldEntries dbEntries) = do
        -- Write file header: magic string and format version
        put magicString
        put version
        -- Write all auxiliary metadata fields (Redis version, architecture, etc.)
        mapM_ put auxFieldEntries
        -- Write all database entries with their key-value pairs
        mapM_ put dbEntries
        -- Write EOF marker to signal end of data
        put EOF
        putWord64le (fromChecksum defaultChecksum)

    get = do
        (rdbFile, generatedChecksum) <- execSGetWithChecksum $ do
            magicString <- getWithChecksum @RDBMagicString
            version <- getWithChecksum @RDBVersion
            auxFieldEntries <- many $ getWithChecksum @AuxField -- Parse until non-aux field
            dbEntries <- manyTill (getWithChecksum @RDbEntry) (getWithChecksum @EOF) -- Parse database entries until EOF marker
            pure $ RDBFile magicString version auxFieldEntries dbEntries

        mChecksum <- optional (toChecksum <$> getWord64le)

        case mChecksum of
            Nothing -> pure rdbFile -- No checksum present, assume valid
            Just checksum ->
                if (checksum == defaultChecksum) || (checksum == generatedChecksum)
                    then pure rdbFile
                    else fail "RDB file checksum mismatch"

instance SBinary RDBFileWithChecksum where
    putWithChecksum (RDBFileWithChecksum (RDBFile magicString version auxFieldEntries dbEntries)) = do
        putWithChecksum magicString
        putWithChecksum version
        mapM_ putWithChecksum auxFieldEntries
        mapM_ putWithChecksum dbEntries
        putWithChecksum EOF -- Write EOF marker
        generatedChecksum <- State.get
        lift $ putWord64le (fromChecksum generatedChecksum)

    -- \| Implemented solely for completeness, if we are truly looking to de-serialize an RDB file we should utilize the Binary instance on the RDBFile type instead
    getWithChecksum = do
        magicString <- getWithChecksum @RDBMagicString
        version <- getWithChecksum @RDBVersion
        auxFieldEntries <- many $ getWithChecksum @AuxField
        dbEntries <- manyTill (getWithChecksum @RDbEntry) (getWithChecksum @EOF) -- Parse database entries until EOF marker
        generatedChecksum <- State.get
        checksum <- lift (toChecksum <$> getWord64le) -- There should always be a checksum at the end of the rdbFile, even if it is just 0
        if checksum == generatedChecksum
            then pure $ RDBFileWithChecksum (RDBFile magicString version auxFieldEntries dbEntries)
            else fail "RDB file checksum mismatch"

{- | Database entry encoding/decoding.
Each database starts with SelectDB, followed by ResizeDB hints, then key-value pairs.
-}
instance SBinary RDbEntry where
    putWithChecksum (RDbEntry entryId resizeDBEntry keyValEntries) = do
        putWithChecksum entryId
        putWithChecksum resizeDBEntry
        mapM_ putWithChecksum keyValEntries

    getWithChecksum = do
        entryId <- getWithChecksum @SelectDB <|> pure def -- Default to SelectDB 0 if not present
        resizeDBEntry <- getWithChecksum @ResizeDB <|> pure def -- Default to ResizeDB 0 0 if not present
        keyValEntries <- many $ getWithChecksum @KeyValueOpCode
        pure $ RDbEntry entryId resizeDBEntry keyValEntries

-- | RDB version is stored as 4 ASCII bytes (e.g., "0003")
instance SBinary RDBVersion where
    putWithChecksum (RDBVersion version) = genericPutWithChecksumUsing (putByteString version)
    getWithChecksum = RDBVersion <$> genericGetWithChecksumUsing (getByteString 4)

-- | Magic string is exactly "REDIS" (5 ASCII bytes)
instance SBinary RDBMagicString where
    putWithChecksum Redis = genericPutWithChecksumUsing (putByteString "REDIS")
    getWithChecksum = do
        magic <- genericGetWithChecksumUsing (getByteString 5)
        if magic == "REDIS"
            then pure Redis
            else fail "Invalid RDB magic string"

-- | KeyValueOpCode encoding with proper opcode prefixes
instance SBinary KeyValueOpCode where
    putWithChecksum = \case
        FCOpCode keyValWithExpiryInMS -> putWithChecksum keyValWithExpiryInMS
        KeyValOpCode keyValWithNoExpiryInfo -> putWithChecksum keyValWithNoExpiryInfo
        FDOpcode keyValWithExpiryInS -> putWithChecksum keyValWithExpiryInS
    getWithChecksum =
        -- Try parsing different key-value types in order of specificity
        asum
            [ FDOpcode <$> getWithChecksum
            , FCOpCode <$> getWithChecksum
            , KeyValOpCode <$> getWithChecksum -- Should be last since it's most permissive
            ]

-- | EOF marker is a single 0xFF byte
instance SBinary EOF where
    putWithChecksum EOF = genericPutWithChecksum @Word8 eofOpCodeByteTag
    getWithChecksum = do
        tag <- genericGetWithChecksum @Word8
        if tag == eofOpCodeByteTag
            then pure EOF
            else fail ("Expected EOF marker got " <> show tag <> " instead")

-- | Millisecond expiry: 0xFC + 8-byte little-endian timestamp + value type + key + value
instance SBinary KeyValWithExpiryInMS where
    putWithChecksum (KeyValWithExpiryInMS expiryTimeMs valueType encodedKey encodedValue) = do
        genericPutWithChecksum @Word8 keyValWithExpiryInMSOpCodeByteTag
        putWithChecksum expiryTimeMs
        putWithChecksum valueType
        putWithChecksum encodedKey
        putWithChecksum encodedValue

    getWithChecksum = do
        tag <- genericGetWithChecksum @Word8
        if tag /= keyValWithExpiryInMSOpCodeByteTag
            then fail ("Expected KeyValWithExpiryInMS marker got " <> show tag <> " instead")
            else KeyValWithExpiryInMS <$> getWithChecksum <*> getWithChecksum <*> getWithChecksum <*> getWithChecksum

-- | Second expiry: 0xFD + 4-byte little-endian timestamp + value type + key + value
instance SBinary KeyValWithExpiryInS where
    putWithChecksum (KeyValWithExpiryInS expiryTimeS valueType encodedKey encodedValue) = do
        genericPutWithChecksum @Word8 keyValWithExpiryInSpCodeByteTag
        putWithChecksum expiryTimeS
        putWithChecksum valueType
        putWithChecksum encodedKey
        putWithChecksum encodedValue

    getWithChecksum = do
        tag <- genericGetWithChecksum @Word8
        if tag /= keyValWithExpiryInSpCodeByteTag
            then fail ("Expected KeyValWithExpiryInS marker got " <> show tag <> " instead")
            else KeyValWithExpiryInS <$> getWithChecksum <*> getWithChecksum <*> getWithChecksum <*> getWithChecksum

-- | No expiry format: value type + key + value (no opcode prefix)
instance SBinary KeyValWithNoExpiryInfo where
    putWithChecksum (KeyValWithNoExpiryInfo valueType encodedKey encodedValue) = do
        putWithChecksum valueType
        putWithChecksum encodedKey
        putWithChecksum encodedValue

    getWithChecksum = KeyValWithNoExpiryInfo <$> getWithChecksum <*> getWithChecksum <*> getWithChecksum

{- | Auxiliary field encoding with 0xFA opcode prefix.
Format: [0xFA][key-string][value-string]
This handles all auxiliary field types with their specific encodings.
-}
instance SBinary AuxField where
    putWithChecksum x = do
        genericPutWithChecksum @Word8 auxiliaryFieldOpCodeByteTag -- Redis auxiliary field opcode
        case x of
            AuxFieldRedisVer ver -> putWithChecksum ver
            AuxFieldRedisBits bits -> putWithChecksum bits
            AuxFieldCTime time -> putWithChecksum time
            AuxFieldUsedMem mem -> putWithChecksum mem
            AuxFieldCustom key value -> do
                putWithChecksum key
                putWithChecksum value

    getWithChecksum = do
        tag <- genericGetWithChecksum @Word8
        if tag /= auxiliaryFieldOpCodeByteTag
            then fail ("Expected AuxField marker got " <> show tag <> " instead")
            else do
                asum
                    [ AuxFieldRedisVer <$> getWithChecksum @RedisVersion
                    , AuxFieldRedisBits <$> getWithChecksum @RedisBits
                    , AuxFieldCTime <$> getWithChecksum @CTime
                    , AuxFieldUsedMem <$> getWithChecksum @UsedMem
                    , AuxFieldCustom <$> getWithChecksum @RDBLengthPrefixedString <*> getWithChecksum @RDBLengthPrefixedVal
                    , -- Custom auxiliary fields can be any key-value pair, so we use RDBLengthPrefixedString for both key and value
                      -- This allows for extensibility in future Redis versions or custom applications
                      -- How can we handle unknown auxiliary fields? Particularly, how can we skip them without failing?
                      fail "Unknown auxiliary field with unsupported value encoding"
                    ]

{- | Redis architecture bits encoding (32-bit or 64-bit).
Stores "redis-bits" key with "32" or "64" value as variable-length strings.
-}
instance SBinary RedisBits where
    putWithChecksum x = do
        -- Write "redis-bits" key followed by architecture value
        putWithChecksum (RDBLengthPrefixedShortString "redis-bits")
        case x of
            RedisBits32 -> putWithChecksum (toRDBLengthPrefixedValOptimizedToIntEncodingIfPossible "32")
            RedisBits64 -> putWithChecksum (toRDBLengthPrefixedValOptimizedToIntEncodingIfPossible "64")
    getWithChecksum = do
        -- Parse and validate "redis-bits" key
        key <- getWithChecksum @RDBLengthPrefixedShortString -- "redis-bits" is 10 bytes
        if key /= RDBLengthPrefixedShortString "redis-bits"
            then fail ("Expected redis-bits marker got " <> show key <> " instead")
            else do
                -- Parse architecture value ("32" or "64")
                tag <- getWithChecksum @RDBLengthPrefixedVal
                case fromRDBLengthPrefixedVal tag of
                    "32" -> pure RedisBits32
                    "64" -> pure RedisBits64
                    _ -> fail "Unknown redis-bits aux field value"

{- | Redis version string encoding.
Stores "redis-ver" key with version string value (e.g., "7.0.0").
-}
instance SBinary RedisVersion where
    putWithChecksum (RedisVersion version) = do
        -- Write "redis-ver" key followed by version string
        putWithChecksum (RDBLengthPrefixedShortString "redis-ver")
        putWithChecksum version
    getWithChecksum = do
        -- Parse and validate "redis-ver" key
        key <- getWithChecksum @RDBLengthPrefixedShortString -- "redis-ver" is 10 bytes
        if key /= RDBLengthPrefixedShortString "redis-ver"
            then fail ("Expected redis-ver marker got " <> show key <> " instead")
            -- Parse version string value
            else RedisVersion <$> getWithChecksum @RDBLengthPrefixedShortString

{- | Creation time (ctime) encoding as Unix timestamp.
Stores "ctime" key with POSIXTime formatted as string value.
-}
instance SBinary CTime where
    putWithChecksum (CTime cTime) = do
        -- Write "ctime" key followed by timestamp as string
        putWithChecksum (RDBLengthPrefixedShortString "ctime")
        putWithChecksum (toRDBLengthPrefixedValOptimizedToIntEncodingIfPossible (BSC.pack . show $ cTime.getRDBUnixTimestampS))

    -- \^ Using the Data.ByteString.Lazy.Char8 to pack to go from string to ByteString since we expect (show posixTime) to be an ASCII string
    -- We also convert to string like is done here: https://github.com/redis/redis/blob/f6f16746e1d4bc51960158d9a896e1aa0a2c7dbd/src/rdb.c#L1257
    getWithChecksum = do
        -- Parse and validate "ctime" key
        key <- getWithChecksum @RDBLengthPrefixedShortString -- "ctime" is 5 bytes
        if key /= RDBLengthPrefixedShortString "ctime"
            then fail ("Expected ctime marker got " <> show key <> " instead")
            else do
                -- Parse timestamp string and convert to numeric value
                timeStr <- getWithChecksum @RDBLengthPrefixedVal
                let cTimeM = BSC.readWord32 . fromRDBLengthPrefixedVal $ timeStr
                -- \^ Using the Data.ByteString.Lazy.Char8 to unpack to string since we expect an ASCII string
                case cTimeM of
                    Just (cTime, "") -> pure $ CTime (RDBUnixTimestampS cTime)
                    _ -> fail "Invalid POSIX time format in ctime field"

{- | Memory usage (used-mem) encoding in bytes.
Stores "used-mem" key with memory usage as variable-length string value.
-}
instance SBinary UsedMem where
    putWithChecksum (UsedMem mem) = do
        -- Write "used-mem" key followed by memory value as string
        putWithChecksum (RDBLengthPrefixedShortString "used-mem")
        putWithChecksum (toRDBLengthPrefixedValOptimizedToIntEncodingIfPossible (BSC.pack . show $ mem))

    getWithChecksum = do
        -- Parse and validate "used-mem" key
        key <- getWithChecksum @RDBLengthPrefixedShortString -- "used-mem" is 8
        if key /= RDBLengthPrefixedShortString "used-mem"
            then fail ("Expected used-mem marker got " <> show key <> " instead")
            else do
                -- Parse memory value string and convert to integer
                memVal <- fromRDBLengthPrefixedVal <$> getWithChecksum @RDBLengthPrefixedVal
                case BSC.readInt memVal of
                    Just (memInt, "") -> pure $ UsedMem memInt
                    _ -> fail "Invalid used-mem format: expected an integer"

{- | Database selection (SelectDB) encoding with 0xFE opcode.
Format: [0xFE][database-index-as-string]
Indicates which Redis database the following entries belong to.
-}
instance SBinary SelectDB where
    putWithChecksum (SelectDB dbIndex) = do
        -- Write database selection opcode
        genericPutWithChecksum @Word8 selectDBOpCodeByteTag -- Redis SelectDB opcode
        -- Write database index using variable-length encoding
        putWithChecksum (toUIntValInRDBLengthPrefixedForm dbIndex)
    getWithChecksum = do
        -- Read and validate database selection opcode
        tag <- genericGetWithChecksum @Word8
        if tag /= selectDBOpCodeByteTag
            then fail ("Expected SelectDB marker got " <> show tag <> " instead")
            else do
                -- Parse database index from variable-length encoding
                dbIndex <- getWithChecksum @UIntValInRDBLengthPrefixForm
                pure $ SelectDB (fromUIntValInRDBLengthPrefixedForm dbIndex)

{- | Database resize hints (ResizeDB) encoding with 0xFB opcode.
Format: [0xFB][total-keys-hint][expiry-keys-hint]
Provides memory allocation hints for hash table sizing optimization.
-}
instance SBinary ResizeDB where
    putWithChecksum (ResizeDB numOfKeys numOfKeysWithExpiry) = do
        genericPutWithChecksum @Word8 resizeDBOpCodeByteTag -- Redis ResizeDB opcode
        putWithChecksum (toUIntValInRDBLengthPrefixedForm numOfKeys)
        putWithChecksum (toUIntValInRDBLengthPrefixedForm numOfKeysWithExpiry)

    getWithChecksum = do
        tag <- genericGetWithChecksum @Word8
        if tag /= resizeDBOpCodeByteTag
            then fail ("Expected ResizeDB marker got " <> show tag <> " instead")
            else do
                numOfKeys <- getWithChecksum @UIntValInRDBLengthPrefixForm
                numOfKeysWithExpiry <- getWithChecksum @UIntValInRDBLengthPrefixForm
                pure $ ResizeDB (fromUIntValInRDBLengthPrefixedForm numOfKeys) (fromUIntValInRDBLengthPrefixedForm numOfKeysWithExpiry)

{- | Redis value type encoding for RDB format.
Each type has a specific byte identifier followed by type-specific data.
Currently only string type (0) is implemented in this codebase.
-}
instance SBinary ValueType where
    putWithChecksum = \case
        Str -> genericPutWithChecksum @Word8 stringValueTypeByteId -- Redis string type identifier
    getWithChecksum = do
        tag <- genericGetWithChecksum @Word8
        case tag of
            0 -> pure Str
            _ -> fail ("Unknown ValueType: " <> show tag)

-- -------------------------------------------------------------------------- --
--                Serialization functions with checksum support               --
-- -------------------------------------------------------------------------- --

-- The reason we're putting and getting with checksum this way is so each sub-component of each RDB file component can contribute to the final checksum, basically we're doing it this way to give the checksum a greater level of granularity and to ensure that the checksum includes all sub-components of a given section, not just the section as a whole. I think this is how redis does it
-- We do not need to do this for RDB File components that are basic/simple, that is, those that do not themselves have sub components like the version, magic string, and eof marker
-- putKeyValWithExpiryMSWithChecksum :: KeyValWithExpiryInMS -> SPut
-- putKeyValWithExpiryMSWithChecksum (KeyValWithExpiryInMS expiryTimeMs valueType encodedKey encodedValue) = do
--     putWithChecksum keyValWithExpiryInMSOpCodeByteTag
--     putWithChecksum expiryTimeMs
--     putWithChecksum valueType
--     putWithChecksum encodedKey
--     putWithChecksum encodedValue

-- getKeyValWithExpiryMSWithChecksum :: SGet KeyValWithExpiryInMS
-- getKeyValWithExpiryMSWithChecksum = do
--     tag <- getWithChecksum
--     if tag /= keyValWithExpiryInMSOpCodeByteTag
--         then fail ("Expected KeyValWithExpiryInMS marker got " <> show tag <> " instead")
--         else KeyValWithExpiryInMS <$> getWithChecksum <*> getWithChecksum <*> getWithChecksum <*> getWithChecksum

-- putKeyValWithExpirySWithChecksum :: KeyValWithExpiryInS -> SPut
-- putKeyValWithExpirySWithChecksum (KeyValWithExpiryInS expiryTimeS valueType encodedKey encodedValue) = do
--     putWithChecksum expiryTimeS
--     putWithChecksum valueType
--     putWithChecksum encodedKey
--     putWithChecksum encodedValue

-- getKeyValWithExpirySWithChecksum :: SGet KeyValWithExpiryInS
-- getKeyValWithExpirySWithChecksum = do
--     expiryTimeS <- getWithChecksum
--     valueType <- getWithChecksum
--     encodedKey <- getWithChecksum
--     KeyValWithExpiryInS expiryTimeS valueType encodedKey <$> getWithChecksum

-- putKeyValWithNoExpiryInfoWithChecksum :: KeyValWithNoExpiryInfo -> SPut
-- putKeyValWithNoExpiryInfoWithChecksum (KeyValWithNoExpiryInfo valueType encodedKey encodedValue) = do
--     putWithChecksum valueType
--     putWithChecksum encodedKey
--     putWithChecksum encodedValue

-- getKeyValWithNoExpiryInfoWithChecksum :: SGet KeyValWithNoExpiryInfo
-- getKeyValWithNoExpiryInfoWithChecksum = do
--     valueType <- getWithChecksum
--     encodedKey <- getWithChecksum
--     KeyValWithNoExpiryInfo valueType encodedKey <$> getWithChecksum

-- putKeyValueOpCodeWithChecksum :: KeyValueOpCode -> SPut
-- putKeyValueOpCodeWithChecksum = \case
--     FCOpCode keyValWithExpiryInMS -> putWithChecksum keyValWithExpiryInMS
--     KeyValOpCode keyValWithNoExpiryInfo -> putWithChecksum keyValWithNoExpiryInfo
--     FDOpcode keyValWithExpiryInS -> putWithChecksum keyValWithExpiryInS

-- getKeyValueOpCodeWithChecksum :: SGet KeyValueOpCode
-- getKeyValueOpCodeWithChecksum =
--     asum
--         [ FDOpcode <$> getKeyValWithExpirySWithChecksum
--         , FCOpCode <$> getKeyValWithExpiryMSWithChecksum
--         , KeyValOpCode <$> getKeyValWithNoExpiryInfoWithChecksum
--         ]

-- putAuxFieldWithChecksum :: AuxField -> SPut
