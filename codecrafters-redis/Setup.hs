import Control.Exception (SomeException, catch)
import Data.Bool (bool)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.String (IsString (..))
import Distribution.PackageDescription (HookedBuildInfo, emptyHookedBuildInfo)
import Distribution.Simple (Args, defaultMainWithHooks, preBuild, simpleUserHooks)
import Distribution.Simple.Setup (BuildFlags)
import Redis.RDB.CRC64 (CheckSum, crc64, fromChecksum)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Process (readProcess)
import Text.Read (readMaybe)

main :: IO ()
main =
    defaultMainWithHooks
        simpleUserHooks
            { preBuild = generateBuildInfo
            }

-- Based off https://github.com/redis/redis/blob/47c51369eeffd55e1baf20df7955a3dfbe842fc4/src/mkreleasehdr.sh for implementing INFO command's server section fields related to git and build info
generateBuildInfo :: Args -> BuildFlags -> IO HookedBuildInfo
generateBuildInfo _ _ = do
    createDirectoryIfMissing True "generated"

    gitSha1 <- gitOr "00000000" ["rev-parse", "--short=8", "HEAD"]
    gitDirty <- gitDirty

    buildId <- show . fromChecksum . crc64 0 . fromString . trim <$> (readProcess "./scripts/mkBuildId.sh" [] "" `catch` \(_ :: SomeException) -> genSimpleBuildId)

    let contents =
            unlines
                [ "module BuildInfo (gitSha1, gitDirty, buildId) where"
                , ""
                , "import Data.String (IsString)"
                , ""
                , "gitSha1 :: (IsString a) => a"
                , "gitSha1 = " <> show gitSha1
                , ""
                , "gitDirty :: Bool"
                , "gitDirty = " <> show gitDirty
                , ""
                , "buildId :: (IsString a) => a"
                , "buildId = " <> show buildId
                ]

    let path = "generated/BuildInfo.hs"

    currGitSha1 <- trim <$> readProcess "grep" ["-m1", "-oP", "^gitSha1\\s*=\\s*\"\\K[^\"]+(?=\")", path] "" `catch` \(_ :: SomeException) -> pure ""
    currGitDirtyM <- readMaybe @Bool . trim <$> readProcess "grep" ["-m1", "-oP", "^gitDirty\\s*=\\s*\\K(True|False)", path] "" `catch` \(_ :: SomeException) -> pure ""

    let isUnchanged = maybe False (\currGitDirty -> currGitSha1 == gitSha1 && currGitDirty == gitDirty) currGitDirtyM

    if isUnchanged
        then putStrLn "Build info is up to date, skipping regeneration."
        else writeFile path contents
    pure emptyHookedBuildInfo

genSimpleBuildId :: IO String
genSimpleBuildId = do
    uname <- trim <$> readProcess "uname" ["-n"] ""
    shellDate <- trim <$> readProcess "date" ["+%s"] ""

    pure $ uname <> "-" <> shellDate

gitOr :: String -> [String] -> IO String
gitOr fallback args =
    trim
        <$> readProcess "git" args ""
            `catch` \(_ :: SomeException) -> pure fallback

gitDirty :: IO Bool
gitDirty =
    fmap (not . null . trim) $
        readProcess "git" ["status", "--porcelain"] ""
            `catch` \(_ :: SomeException) -> pure ""

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
