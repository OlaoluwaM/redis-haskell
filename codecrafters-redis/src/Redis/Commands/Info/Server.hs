module Redis.Commands.Info.Server (
    genServerInfoSectionStr,
    MkServerInfoArgs (..),
) where

import BuildInfo qualified as Redis.BuildInfo
import Data.ByteString.Base16 qualified as Base16

import Data.Base16.Types (extractBase16)
import Data.Bits (FiniteBits (finiteBitSize))
import Data.Int (Int64)
import Data.String (fromString)
import Data.Text (Text)
import Data.Time (UTCTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Version (showVersion)
import Data.Word (Word16, Word64)
import Effectful (Eff, (:>))
import Redis.Commands.Info.Common (formatToInfoSectionField, maskFieldWhenNecessary)
import Redis.Effect.Time (Time, getCurrentTime)
import Redis.Server.Metadata (Environment)
import Redis.Server.Version (redisVersion)
import System.Entropy (getEntropy, getHardwareEntropy)
import System.Environment (getProgName)
import System.IO.Unsafe (unsafePerformIO)
import System.Info (fullCompilerVersion, os)
import System.Posix (ProcessID, SystemID (..), getProcessID, getSystemID)

data ServerInfo = ServerInfo
    { redisVersion :: Text -- Get from Version.hs
    , redisGitSha1 :: Text -- Get from BuildInfo.hs
    , redisGitDirty :: Bool -- Get from BuildInfo.hs
    , redisBuildId :: Text -- Get from BuildInfo.hs
    , redisMode :: Text -- Values are "standalone", "sentinel", "cluster", default to "standalone"
    , os :: Text -- Use https://www.stackage.org/haddock/lts-24.38/unix-2.8.7.0/System-Posix-Unistd.html#v:getSystemID to get this value similar to what redis does https://github.com/redis/redis/blob/47c51369eeffd55e1baf20df7955a3dfbe842fc4/src/server.c#L6331. Example values are "Linux 5.15.0-1051-azure x86_64" or "Darwin 22.6.0 x86_64"
    , archBits :: ArchBits -- Values are 32 or 64, default to 64. Redis does a check on the size of a long to determine this value, but we'll just use 64 since that's what GHC seems to mainly support
    , monotonicClock :: Text -- Tells us what monotonic timer source redis is using for measuring elapsed time. In our case, have it be to "POSIXTime getCurrentTime"
    , multiplexingApi :: Text -- Values are "epoll", "kqueue", "select", "poll", default is "select". This field notifies us of the async IO (IO multiplexing) backend that Redis uses.
    , atomicVarApi :: Text -- Tells us what API for atomic operations implementation redis is using. Discovery uses a similar approach to multiplexing_api in Redis. For our case, this will be fixed to STM
    , ghcVersion :: Text -- Redis has a gcc_version field, but this is Haskell so we will have a ghcVersion field instead.
    , processId :: ProcessID -- Can be attained using https://www.stackage.org/haddock/lts-24.38/unix-2.8.7.0/System-Posix-Process.html
    , processSupervised :: Text -- Values are "upstart", "systemd", "unknown", and "no". Redis will inspect the OS for this value. We will default to "unknown" and thus False
    , runId :: Text -- A random unique (per startup) 40-char long hex string ID.
    , tcpPort :: Text -- Port server is running on
    , serverTimeUsec :: Word -- Current time in unix timestamp in microseconds precision
    , uptimeInSeconds :: Word -- Number of seconds since the server started
    , uptimeInDays :: Word16
    , hz :: Word -- Cron frequency. Tells us how often (cron) background jobs are performed. Values is X times per second. Since we don't have any cron stuff we will just set this to 0: https://github.com/redis/redis/blob/47c51369eeffd55e1baf20df7955a3dfbe842fc4/src/server.c#L1216
    , configuredHz :: Word -- Same as above, but static from the configuration. Redis can dynamically alter cron frequency depending on a number of factors. This will also default to 0
    , lruClock :: Word64 -- Current redis LRU time counter. We don't have an LRU clock so default is 0
    , executable :: Text -- name of the executable
    , configFile :: Text -- config file path
    , ioThreadsActive :: Bool -- Indicates whether redis is using I/O threads or async IO for client I/O operations. Defaults to True since Haskell is multithreaded by default
    }

data ArchBits = Bits32 | Bits64
    deriving stock (Eq, Show)

data MkServerInfoArgs = MkServerInfoArgs
    { configFilePath :: FilePath
    , startupTime :: UTCTime
    , tcpPort :: Text
    }

-- In C, Redis determines the architecture bits by checking the size of a long integer (https://github.com/redis/redis/blob/47c51369eeffd55e1baf20df7955a3dfbe842fc4/src/server.c#L6331). In Haskell, we can do something similar using the`finiteBitSize` function from the `Data.Bits` to determine the number of bits in an `Int`, which is typically sized by the platform.
{-# NOINLINE archBits #-}
archBits :: ArchBits
archBits = case finiteBitSize (0 :: Int) of
    32 -> Bits32
    64 -> Bits64
    _ -> Bits64 -- Default to 64 if we can't determine it, since that's the most common architecture nowadays

-- The Redis implementation does a better job of figuring this out. Redis will probe the OS (checking sys calls and the like) at build time, define macros, then based on those macros it will conditionally load an associated api file: https://github.com/redis/redis/blob/47c51369eeffd55e1baf20df7955a3dfbe842fc4/src/ae.c#L32-L44 . By comparison, what we have below is a bit measly but should suffice since it relies on the usual rule of thumb for these things.
{-# NOINLINE multiplexingApi #-}
multiplexingApi :: Text
multiplexingApi = case os of
    "linux" -> "epoll"
    "darwin" -> "kqueue"
    "freebsd" -> "kqueue"
    "openbsd" -> "kqueue"
    "netbsd" -> "kqueue"
    "solaris2" -> "evport"
    _ -> "select"

{-# NOINLINE ghcVersion #-}
ghcVersion :: Text
ghcVersion = fromString $ showVersion fullCompilerVersion

-- We are using `unsafePerformIO` here because we want this to be a constant value that only changes when we restart the server
-- Implementation derived from https://hackage.haskell.org/package/random-string-0.1.0.1/docs/src/System-RandomString.html#randomString
{-# NOINLINE runId #-} -- Prevent inlining instructing GHC to treat this as a single global binding as we want it to be. Otherwise GHC could inline this at call sites causing the expression to be evaluated multiple times which is not what we want
runId :: Text
runId = unsafePerformIO $ do
    let getE n = maybe (getEntropy n) pure =<< getHardwareEntropy n
    let enc = Base16.encodeBase16 -- runId needs to be a hex string: https://github.com/redis/redis/blob/47c51369eeffd55e1baf20df7955a3dfbe842fc4/src/util.c#L1020
    let strLen = 20 -- A run ID is fixed to 40 chars in length. We use 20 because with a base 16 encoding each byte becomes two hex characters, one hex char for the top 4 bits (a nibble) and another for the bottom 4 bits: https://github.com/redis/redis/blob/47c51369eeffd55e1baf20df7955a3dfbe842fc4/src/server.c#L2335 & https://github.com/redis/redis/blob/47c51369eeffd55e1baf20df7955a3dfbe842fc4/src/server.h#L131
    extractBase16 . enc <$> getE strLen

{-# NOINLINE executableName #-}
executableName :: Text
executableName = fromString $ unsafePerformIO getProgName

microSecondsSinceEpoch :: UTCTime -> Int64
microSecondsSinceEpoch = floor . (1e6 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

secondsSinceEpoch :: UTCTime -> Int64
secondsSinceEpoch = floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

-- Since these values don't change until the server restarts, we can cache them in global constants using `unsafePerformIO` to avoid having to recompute them on every call to `mkServerInfo`.
{-# NOINLINE osInfo #-}
osInfo :: Text
osInfo = unsafePerformIO $ do
    rawSysInfo <- getSystemID
    let sysName = rawSysInfo.systemName
        release = rawSysInfo.release
        machine = rawSysInfo.machine
    pure $ fromString (sysName <> " " <> release <> " " <> machine)

{-# NOINLINE processId #-}
processId :: ProcessID
processId = unsafePerformIO getProcessID

-- Having the bindings for the fields be defined outside of the `mkServerInfo` expression allows to cache the values of these fields at startup and reuse them across calls to `mkServerInfo` without having to recompute them each time.
-- TODO: Add uptime to server context as metadata. For this we only need to note the timestamp at startup. Also include the path to the config file
mkServerInfo :: (Time :> es) => MkServerInfoArgs -> Eff es ServerInfo
mkServerInfo MkServerInfoArgs{configFilePath, startupTime, tcpPort} = do
    currTime <- getCurrentTime
    let uptimeInSeconds = secondsSinceEpoch currTime - secondsSinceEpoch startupTime
    let secondsInDay = 60 * 60 * 24
    pure
        ServerInfo
            { redisVersion = redisVersion
            , redisGitSha1 = Redis.BuildInfo.gitSha1
            , redisGitDirty = Redis.BuildInfo.gitDirty
            , redisBuildId = Redis.BuildInfo.buildId
            , redisMode = "standalone"
            , os = osInfo
            , archBits = archBits
            , monotonicClock = "POSIXTime getCurrentTime"
            , multiplexingApi = multiplexingApi
            , atomicVarApi = "STM"
            , ghcVersion = ghcVersion
            , processId = processId
            , processSupervised = "unknown"
            , runId = runId
            , tcpPort = tcpPort
            , serverTimeUsec = fromIntegral $ microSecondsSinceEpoch currTime
            , uptimeInSeconds = fromIntegral uptimeInSeconds
            , uptimeInDays = fromIntegral $ uptimeInSeconds `div` secondsInDay
            , hz = 0
            , configuredHz = 0
            , lruClock = 0
            , executable = executableName
            , configFile = fromString configFilePath
            , ioThreadsActive = True
            }

mkServerInfoSectionStr :: Environment -> ServerInfo -> Text
mkServerInfoSectionStr serverEnv info =
    let formatGitDirty dirty = if dirty then "1" else "0"
        formatArchBits bits = case bits of Bits32 -> "32"; Bits64 -> "64"
        formatThreadsActive active = if active then "1" else "0"
        maskFieldDuringTesting = maskFieldWhenNecessary serverEnv
     in "# Server\r\n"
            <> mconcat
                [ formatToInfoSectionField "redis_version" info.redisVersion
                , formatToInfoSectionField "redis_git_sha1" (maskFieldDuringTesting info.redisGitSha1)
                , formatToInfoSectionField "redis_git_dirty" (maskFieldDuringTesting $ formatGitDirty info.redisGitDirty)
                , formatToInfoSectionField "redis_build_id" (maskFieldDuringTesting info.redisBuildId)
                , formatToInfoSectionField "redis_mode" info.redisMode
                , formatToInfoSectionField "os" (maskFieldDuringTesting info.os)
                , formatToInfoSectionField "arch_bits" (formatArchBits info.archBits)
                , formatToInfoSectionField "monotonic_clock" info.monotonicClock
                , formatToInfoSectionField "multiplexing_api" info.multiplexingApi
                , formatToInfoSectionField "atomicvar_api" info.atomicVarApi
                , formatToInfoSectionField "ghc_version" (maskFieldDuringTesting info.ghcVersion) -- ghc not gcc. This is Haskell
                , formatToInfoSectionField "process_id" (maskFieldDuringTesting . fromString . show $ info.processId)
                , formatToInfoSectionField "process_supervised" info.processSupervised
                , formatToInfoSectionField "run_id" (maskFieldDuringTesting info.runId)
                , formatToInfoSectionField "tcp_port" info.tcpPort
                , formatToInfoSectionField "server_time_usec" (maskFieldDuringTesting . fromString . show $ info.serverTimeUsec)
                , formatToInfoSectionField "uptime_in_seconds" (maskFieldDuringTesting . fromString . show $ info.uptimeInSeconds)
                , formatToInfoSectionField "uptime_in_days" (maskFieldDuringTesting . fromString . show $ info.uptimeInDays)
                , formatToInfoSectionField "hz" (fromString $ show info.hz)
                , formatToInfoSectionField "configured_hz" (fromString $ show info.configuredHz)
                , formatToInfoSectionField "lru_clock" (fromString $ show info.lruClock)
                , formatToInfoSectionField "executable" info.executable
                , formatToInfoSectionField "config_file" info.configFile
                , formatToInfoSectionField "io_threads_active" (formatThreadsActive info.ioThreadsActive)
                ]

genServerInfoSectionStr :: (Time :> es) => MkServerInfoArgs -> Environment -> Eff es Text
genServerInfoSectionStr mkServerInfoArgs serverEnv = mkServerInfoSectionStr serverEnv <$> mkServerInfo mkServerInfoArgs
