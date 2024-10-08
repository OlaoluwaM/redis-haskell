module RESP.ParserSpec where

import RESP.Parser
import RESP.Types
import Test.Hspec
import Test.QuickCheck

import Data.Text qualified as T
import Data.Vector qualified as V

import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString (ByteString)
import Data.Either (isLeft, isRight)
import Data.Foldable (for_)
import Data.Text (Text)
import Data.String.Interpolate ( i )
import Utils (AlphabeticString (..))

type SerializerTestInput = [(Text, RESPDataType, ByteString)]

genValidRawBulkString :: Gen ByteString
genValidRawBulkString = do
    strLength <- chooseInt (1, 100)
    mainStr <- ((.unAlphabeticString) <$> arbitrary @(AlphabeticString Text)) `suchThat` ((== strLength) . T.length)
    let rawBulkString = [i|$#{strLength}#{seqTerminator}#{mainStr}#{seqTerminator}|]
    pure rawBulkString

genInvalidRawBulkString :: Gen ByteString
genInvalidRawBulkString = do
    (Positive strLength) <- arbitrary @(Positive Int)
    mainStr <- ((.unAlphabeticString) <$> arbitrary @(AlphabeticString Text)) `suchThat` ((/= strLength) . T.length)
    let rawBulkString = [i|$#{strLength}#{seqTerminator}#{mainStr}#{seqTerminator}|]
    pure rawBulkString

spec_tests :: Spec
spec_tests = do
    describe "Bulk string RESP data type parser" do
        it "Succeeds when provided a valid raw bulk string input" $ forAll genValidRawBulkString $ isRight . parseOnly bulkStringParser

        it "Succeeds when provided an empty raw bulk string input" do
            let input = [i|$0#{seqTerminator}#{seqTerminator}|]
            let result = parseOnly bulkStringParser input
            result `shouldSatisfy` isRight

        it "Succeeds when provided a raw bulk string input with spaces" do
            let input = [i|$13#{seqTerminator}Hello, Redis!#{seqTerminator}|]
            let result = parseOnly bulkStringParser input
            result `shouldSatisfy` isRight

        it "Succeeds when provided a null raw bulk string input" do
            let input = [i|$-1#{seqTerminator}|]
            let result = parseOnly bulkStringParser input
            result `shouldSatisfy` isRight

        it "Succeeds even when provided an invalid null raw bulk string input" do
            let input = [i|$-1#{seqTerminator}fefrefer|]
            let result = parseOnly bulkStringParser input
            result `shouldSatisfy` isRight

        it "Fails when provided an invalid raw bulk string input" $ forAll genInvalidRawBulkString $ isLeft . parseOnly bulkStringParser

        for_ [(2 :: Int, "thequickbrownfoxjumpedoverthelazydog" :: Text), (1, "")] $ \(len, str) ->
            it [i|Fails when provided an raw bulk string input with a mismatch between its length and content: len: #{len}, content: #{str} |] do
                let input = [i|$#{len}#{seqTerminator}#{str}#{seqTerminator}|]
                let result = parseOnly bulkStringParser input
                result `shouldSatisfy` isLeft

    describe "RESP data type serializer" do
        let testCasesWithValidRESPDataTypes =
                [ ("a simple string" :: Text, SimpleString "averynaughtyhare", [i|+averynaughtyhare#{seqTerminator}|])
                , ("an empty simple string", SimpleString "", [i|+#{seqTerminator}|])
                , ("a bulk string", MkBulkStringResponse (BulkString "helloworld"), [i|$10#{seqTerminator}helloworld#{seqTerminator}|])
                , ("an empty bulk string", MkBulkStringResponse (BulkString ""), [i|$0#{seqTerminator}#{seqTerminator}|])
                , ("an null bulk string", MkBulkStringResponse NullBulkString, [i|$-1#{seqTerminator}|])
                , ("a positive RESP Integer", RESPInteger 99, [i|:99#{seqTerminator}|])
                , ("a negative RESP Integer", RESPInteger (-34), [i|:-34#{seqTerminator}|])
                , ("a null array", MkArrayResponse NullArray, [i|*-1#{seqTerminator}|])
                , ("a null value", Null, [i|_#{seqTerminator}|])
                ,
                    ( "a flat array"
                    , MkArrayResponse . Array . V.fromList $ [MkBulkStringResponse . BulkString $ "hello", RESPInteger 4, MkBulkStringResponse NullBulkString]
                    , [i|*3#{seqTerminator}$5#{seqTerminator}hello#{seqTerminator}:4#{seqTerminator}$-1#{seqTerminator}|]
                    )
                ,
                    ( "a nested array"
                    , MkArrayResponse . Array . V.fromList $
                        [ MkArrayResponse . Array . V.fromList $ [RESPInteger 1, RESPInteger 2, RESPInteger 3]
                        , MkArrayResponse . Array . V.fromList $ [SimpleString "Hello", SimpleString "World"]
                        ]
                    , "*2\r\n*3\r\n:1\r\n:2\r\n:3\r\n*2\r\n+Hello\r\n+World\r\n"
                    )
                ]

        for_ testCasesWithValidRESPDataTypes $ \(testName, input, expected) ->
            it [i|Succeeds when passed #{testName}|] $ serializeRESPDataType input `shouldBe` expected
