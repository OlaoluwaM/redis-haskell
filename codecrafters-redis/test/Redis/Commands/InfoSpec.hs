module Redis.Commands.InfoSpec where

import Test.Hspec

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.Foldable (for_)
import Data.List (find)
import Data.String (IsString (fromString))
import Data.Traversable (for)
import Path (parseRelFile, reldir, relfile, toFilePath, (</>))
import Redis.Handler (handleCommandReq)
import Redis.Helper (infoCmd, mkBulkString, mkCmdReqStr)
import Redis.Server.Config (
    HostPortRec (..),
    Port,
    RedisConfig,
    RedisConfigF (..),
    ReplicaOfType (..),
 )
import Redis.Server.Config.Defaults (DefaultRedisConfig (..), defaultRedisConfig)
import Redis.Server.Context (ServerContext (..))
import Redis.Test (PassableTestContext (..), runTestServer)
import Rerefined.Refine.TH (refineTH)
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)

{-
  # Test Plan

  1. We should write some tests that ensures listing a section filters out the output of the INFO command
  2. Have a golden/snapshot test to ensure the output structure doesn't change from underneath us. Since some of these info sections include information with some randomization backed in (like the runId) we'd need to parse through the structure and elide those values to avoid constant false positives with the golden tests. Or instead of implementing a masking thingy we just implement a backdoor into the info section str generator function that would allow us override/fix certain field values. Or better yet, expose the runtime env (dev, prod, test) and use that to conditionally masking varying values
  4. I am contemplating doing a golden tests for `INFO` since I may want to flesh out some of the other sections later so such a test might have a high degree of churn
  5. Let's just keep things simple and have a golden test for each section
-}

-- Add tests that assert the following: INFO default == INFO, INFO everything == INFO all (per testing with an actual redis instance)

-- Is there a way to do snapshot testing while ignoring certain details of the snapshot?

-- Runs 'INFO <section>' against a server seeded with the given config and pulls out the value of the named field, if present
runInfoAndExtractField :: RedisConfig -> ByteString -> ByteString -> IO (Maybe ByteString)
runInfoAndExtractField testConfig section fieldName = do
    let cmdReq = mkCmdReqStr [infoCmd, mkBulkString section]
    res <- runTestServer (handleCommandReq @ServerContext cmdReq) (PassableTestContext{config = Just testConfig, serverState = Nothing, metadata = Nothing})
    pure $ extractInfoField fieldName res

-- Pulls the value of a `name:value` field out of a raw INFO response
extractInfoField :: ByteString -> ByteString -> Maybe ByteString
extractInfoField fieldName res = BS.stripPrefix fieldPrefix =<< find (fieldPrefix `BS.isPrefixOf`) (crlfLines res)
  where
    fieldPrefix = fieldName <> ":"
    -- INFO fields are CRLF-terminated, and 'BS8.lines' only splits on LF, so we trim the stray CR off each line
    crlfLines = map (BS8.dropWhileEnd (== '\r')) . BS8.lines

test_info_cmd_golden_test_without_section_list :: IO TestTree
test_info_cmd_golden_test_without_section_list = do
    let cmdReq = mkCmdReqStr [infoCmd]

    let runInfoCmd = runTestServer (handleCommandReq @ServerContext cmdReq) (PassableTestContext{config = Nothing, serverState = Nothing, metadata = Nothing})

    let goldenPath = [relfile|test/Redis/Commands/Info/golden/default-info.txt|]

    pure $ goldenVsString "Golden test for INFO command without section filtering" (toFilePath goldenPath) (fromStrict <$> runInfoCmd)

test_info_cmd_golden_test_with_invalid_sections :: IO TestTree
test_info_cmd_golden_test_with_invalid_sections = do
    let cmdReq = mkCmdReqStr [infoCmd, mkBulkString "fwfw", mkBulkString "9482bd", mkBulkString "wmwownf"]

    let runInfoCmd = runTestServer (handleCommandReq @ServerContext cmdReq) (PassableTestContext{config = Nothing, serverState = Nothing, metadata = Nothing})

    let goldenPath = [relfile|test/Redis/Commands/Info/golden/invalid-sections-info.txt|]

    pure $ goldenVsString "Golden test for INFO command with invalid section filtering" (toFilePath goldenPath) (fromStrict <$> runInfoCmd)

test_info_cmd_golden_test_filtered_by_single_section :: IO [TestTree]
test_info_cmd_golden_test_filtered_by_single_section =
    for
        [
            ( "server"
            , mkBulkString "SeRVer"
            , [relfile|test/Redis/Commands/Info/golden/server-section-info.txt|]
            )
        ,
            ( "replication"
            , mkBulkString "rEplIcAtIon"
            , [relfile|test/Redis/Commands/Info/golden/replication-section-info.txt|]
            )
        ]
        $ \(sectionName, sectionArg, goldenPath) -> do
            let cmdReq = mkCmdReqStr [infoCmd, sectionArg]

            let runInfoCmd = runTestServer (handleCommandReq @ServerContext cmdReq) (PassableTestContext{config = Nothing, serverState = Nothing, metadata = Nothing})

            pure $ goldenVsString ("Golden test for INFO command filtered to just the " <> sectionName <> " section") (toFilePath goldenPath) (fromStrict <$> runInfoCmd)

test_info_cmd_golden_test_filtered_by_multiple_sections :: IO TestTree
test_info_cmd_golden_test_filtered_by_multiple_sections = do
    let cmdReq = mkCmdReqStr [infoCmd, mkBulkString "memory", mkBulkString "clients", mkBulkString "Server", mkBulkString "Keyspace"]

    let runInfoCmd = runTestServer (handleCommandReq @ServerContext cmdReq) (PassableTestContext{config = Nothing, serverState = Nothing, metadata = Nothing})

    let goldenPath = [relfile|test/Redis/Commands/Info/golden/filtered-by-multiple-sections-info.txt|]

    pure $ goldenVsString "Golden test for INFO command filtered by multiple sections" (toFilePath goldenPath) (fromStrict <$> runInfoCmd)

test_info_cmd_golden_test_filtered_by_special_section_keywords :: IO [TestTree]
test_info_cmd_golden_test_filtered_by_special_section_keywords = for ["all" :: String, "everything", "default"] $ \specialSectionKeywordS -> do
    let specialSectionKeywordBS = fromString specialSectionKeywordS
    let cmdReq = mkCmdReqStr [infoCmd, mkBulkString "memory", mkBulkString "clients", mkBulkString specialSectionKeywordBS, mkBulkString "Keyspace"]

    let runInfoCmd = runTestServer (handleCommandReq @ServerContext cmdReq) (PassableTestContext{config = Nothing, serverState = Nothing, metadata = Nothing})

    let rawGoldenPathFilename = "info-with-multiple-sections-and-" <> specialSectionKeywordS <> "-keyword.txt"
    goldenPathFilename <- parseRelFile rawGoldenPathFilename

    let goldenPath = [reldir|test/Redis/Commands/Info/golden|] </> goldenPathFilename

    pure $ goldenVsString ("Golden test for INFO command filtered by multiple sections and the " <> specialSectionKeywordS <> " keyword") (toFilePath goldenPath) (fromStrict <$> runInfoCmd)

spec_info_cmd_golden_default_equivalency :: Spec
spec_info_cmd_golden_default_equivalency = describe "INFO command invocation equivalency tests" $ do
    it "Output of 'INFO' should be equivalent to the output of 'INFO default'" $ do
        -- Per testing with an actual redis server
        let cmdReq1 = mkCmdReqStr [infoCmd]
        let cmdReq2 = mkCmdReqStr [infoCmd, mkBulkString "default"]

        res1 <- runTestServer (handleCommandReq @ServerContext cmdReq1) (PassableTestContext{config = Nothing, serverState = Nothing, metadata = Nothing})
        res2 <- runTestServer (handleCommandReq @ServerContext cmdReq2) (PassableTestContext{config = Nothing, serverState = Nothing, metadata = Nothing})

        res1 `shouldBe` res2

    it "Output of 'INFO all' should be equivalent to the output of 'INFO everything'" $ do
        -- Per testing with an actual redis server
        let cmdReq1 = mkCmdReqStr [infoCmd, mkBulkString "all"]
        let cmdReq2 = mkCmdReqStr [infoCmd, mkBulkString "everything"]

        res1 <- runTestServer (handleCommandReq @ServerContext cmdReq1) (PassableTestContext{config = Nothing, serverState = Nothing, metadata = Nothing})
        res2 <- runTestServer (handleCommandReq @ServerContext cmdReq2) (PassableTestContext{config = Nothing, serverState = Nothing, metadata = Nothing})

        res1 `shouldBe` res2

spec_info_cmd_replication_role :: Spec
spec_info_cmd_replication_role = describe "INFO replication role field" $ do
    let DefaultRedisConfig defaults = defaultRedisConfig

    it "Reports role:master when the server is not a replica of anything" $ do
        role <- runInfoAndExtractField defaults{replicaOf = NO_ONE} "replication" "role"
        role `shouldBe` Just "master"

    it "Reports role:slave when the server is configured as a replica" $ do
        for_
            [ HostPortRec "localhost" ($$(refineTH 6380) :: Port)
            , HostPortRec "10.0.0.2" ($$(refineTH 6379) :: Port)
            ]
            $ \hostPort -> do
                role <- runInfoAndExtractField defaults{replicaOf = HostPort hostPort} "replication" "role"
                role `shouldBe` Just "slave"
