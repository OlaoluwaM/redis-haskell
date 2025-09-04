module Redis.RDB.LZFSpec where

import Redis.RDB.LZF
import Test.Tasty

import Data.ByteString qualified as BS
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Data.ByteString (ByteString)
import Data.String.Interpolate (i)
import Hedgehog (MonadGen)
import Test.Tasty.Hedgehog (testProperty)

test_lzf_compression_prop_tests :: [TestTree]
test_lzf_compression_prop_tests =
    [ testProperty [i|lzf compression does not compress small inputs (<= #{minimumByteStringLength} bytes)|] propLzfDoesNotRoundTripOnSmallInputs
    , testProperty "lzf compression roundtrips" propLzfRoundTripLarge
    ]

propLzfRoundTripLarge :: H.Property
propLzfRoundTripLarge = H.property $ do
    byteString <- H.forAll genLargeByteSequence
    compressThenDecompress byteString H.=== Just byteString

propLzfDoesNotRoundTripOnSmallInputs :: H.Property
propLzfDoesNotRoundTripOnSmallInputs = H.property $ do
    byteString <- H.forAll genSmallByteSequence
    compressThenDecompress byteString H.=== Nothing

genLargeByteSequence :: (MonadGen m) => m ByteString
genLargeByteSequence = do
    let minSize = minimumByteStringLength + 1
    let maxSize = 1000
    Gen.bytes (Range.linear minSize maxSize)

genSmallByteSequence :: (MonadGen m) => m ByteString
genSmallByteSequence = do
    let minSize = 0
    let maxSize = minimumByteStringLength
    Gen.bytes (Range.linear minSize maxSize)

compressThenDecompress :: ByteString -> Maybe ByteString
compressThenDecompress bs = let len = BS.length bs in lzfCompress bs >>= lzfDecompress len
