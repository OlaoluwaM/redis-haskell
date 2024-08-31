module Commands.String.SetSpec where

import Commands.String.Set
import Commands.String.Set.Internal

import Test.Hspec

import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString (ByteString)
import Data.Default (Default (def))
import Data.Either (isLeft)
import Data.Foldable (for_)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Helpers (bulkStrToOptionString)
import RESP.Types (BulkString (BulkString))

spec_tests :: Spec
spec_tests = do
    describe "SET command option parser" do
        for_
            [
                ( "All options set" :: Text
                , bulkStrToOptionString [BulkString "NX", BulkString "GET", BulkString "KEEPTTL", BulkString "9394"]
                , SetCmdOpts{setCondition = OnlyIfKeyDoesNotExist, returnOldVal = True, ttl = KeepTTL}
                )
            ,
                ( "All options set out of order" :: Text
                , bulkStrToOptionString [BulkString "GET", BulkString "EX            90", BulkString "NX"]
                , SetCmdOpts{setCondition = OnlyIfKeyDoesNotExist, returnOldVal = True, ttl = EX (Seconds 90)}
                )
            ,
                ( "Some options set" :: Text
                , bulkStrToOptionString [BulkString "XX", BulkString "EX", BulkString "60"]
                , SetCmdOpts{setCondition = OnlyIfKeyExists, returnOldVal = False, ttl = EX (Seconds 60)}
                )
            ,
                ( "Some options set (2)" :: Text
                , bulkStrToOptionString [BulkString "XX", BulkString "PX", BulkString "610"]
                , SetCmdOpts{setCondition = OnlyIfKeyExists, returnOldVal = False, ttl = PX (MilliSeconds 610)}
                )
            ,
                ( "Some options set (3)" :: Text
                , bulkStrToOptionString [BulkString "EXAT", BulkString "100"]
                , SetCmdOpts{setCondition = Always, returnOldVal = False, ttl = EXAT (UnixTimeSeconds 100)}
                )
            ,
                ( "Some options set (4)" :: Text
                , bulkStrToOptionString [BulkString "NX", BulkString "XX", BulkString "EXAT", BulkString "100"]
                , (def @SetCmdOpts){setCondition = OnlyIfKeyDoesNotExist}
                )
            ,
                ( "Some options set out of order" :: Text
                , bulkStrToOptionString [BulkString "GET", BulkString "EX", BulkString "60"]
                , SetCmdOpts{setCondition = Always, returnOldVal = True, ttl = EX (Seconds 60)}
                )
            ,
                ( "Some options set out of order (2)" :: Text
                , bulkStrToOptionString [BulkString "GET", BulkString "XX"]
                , SetCmdOpts{setCondition = OnlyIfKeyExists, returnOldVal = True, ttl = def @TTL}
                )
            ,
                ( "no options set" :: Text
                , bulkStrToOptionString []
                , def @SetCmdOpts
                )
            ,
                ( "one option set" :: Text
                , bulkStrToOptionString [BulkString "PXAT", BulkString "160"]
                , (def @SetCmdOpts){ttl = PXAT (UnixTimeMilliSeconds 160)}
                )
            ]
            $ \(testDesc, input, expected) ->
                it [i|Can parse a valid sequence of SET command options: #{testDesc}|] do
                    let result = parseOnly parseSetCmdOptions input
                    result `shouldBe` Right expected

        it "Fails when provided invalid ttl time offsets" $ do
            let input = bulkStrToOptionString [BulkString "PX", BulkString "0"]
            let result = parseOnly parseSetCmdOptions input
            result `shouldSatisfy` isLeft

        for_
            [
                ( "(1)" :: Text
                , "fjfrf erfjer  EX fejrfbeoirfn" :: ByteString
                )
            ,
                ( "(2)" :: Text
                , "wfw erfeerf fwerw" :: ByteString
                )
            ]
            $ \(testDesc, input) ->
                it [i|Doesn't fail even if passed an invalid sequence of SET command options: #{testDesc}|] do
                    let result = parseOnly parseSetCmdOptions input
                    result `shouldBe` Right (def @SetCmdOpts)