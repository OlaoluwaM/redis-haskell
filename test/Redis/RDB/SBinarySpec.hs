module Redis.RDB.SBinarySpec where

import Redis.RDB.SBinary
import Test.Hspec

import Data.Binary.Get qualified as Get
import Data.Binary.Put qualified as Put

import Data.Binary.Put (putByteString)
import Redis.RDB.Format (defaultChecksum)

spec_sbinary :: Spec
spec_sbinary = do
    describe "SBinary" do
        it "puts a value with a checksum" do
            let value = "test"
            let (checksum, _) = Put.runPutM . execSPutWithChecksum $ genericPutWithChecksumUsing (putByteString value)
            checksum `shouldNotBe` defaultChecksum

        it "gets a value with a checksum" do
            let value = "test"
            let (checksum, consumedInput) = Put.runPutM . execSPutWithChecksum $ genericPutWithChecksumUsing (putByteString value)

            let (decodedValue, decodedChecksum) = flip Get.runGet consumedInput . execSGetWithChecksum $ genericGetWithChecksumUsing (Get.getByteString 4)

            decodedChecksum `shouldBe` checksum
            decodedValue `shouldBe` value

        it "roundtrips with checksum" $ do
            let (putChecksum, byteSequence) = Put.runPutM . execSPutWithChecksum $ do
                    genericPutWithChecksumUsing (Put.putWord16be 999)
                    genericPutWithChecksumUsing (Put.putWord16be 2334)
                    genericPutWithChecksumUsing (Put.putByteString "frefevrevervec")
                    genericPutWithChecksumUsing (Put.putByteString "everfewefrefwer")
                    genericPutWithChecksumUsing (Put.putByteString "additional data")

            let (decodedValue, decodedChecksum) = flip Get.runGet byteSequence . execSGetWithChecksum $ do
                    v1 <- genericGetWithChecksumUsing Get.getWord16be
                    v2 <- genericGetWithChecksumUsing Get.getWord16be
                    v3 <- genericGetWithChecksumUsing (Get.getByteString 14)
                    v4 <- genericGetWithChecksumUsing (Get.getByteString 15)
                    v5 <- genericGetWithChecksumUsing (Get.getByteString 15)
                    pure (v1, v2, v3, v4, v5)

            (decodedValue, decodedChecksum) `shouldBe` ((999, 2334, "frefevrevervec", "everfewefrefwer", "additional data"), putChecksum)
