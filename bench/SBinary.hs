module SBinary (benchmarks) where

import Criterion.Main
import Redis.RDB.SBinary

import Data.ByteString qualified as BS

import Data.Binary.Get (runGet)
import Data.Binary.Put (runPutM)
import Data.ByteString (ByteString)
import Data.Char (ord)
import Redis.RDB.Data (RDBLengthPrefixedVal, toRDBLengthPrefixedVal)

benchmarks :: [Benchmark]
benchmarks =
    let charCodeForA = fromIntegral $ ord 'a'
     in [ bgroup
            "SBinary on roundtrip RDB length-prefixed values "
            [ bench "small" $ nf encodeThenDecodeRDBLengthPrefixedVal (BS.replicate 100 charCodeForA)
            , bench "medium" $ nf encodeThenDecodeRDBLengthPrefixedVal (BS.replicate 100000 charCodeForA)
            , bench "large" $ nf encodeThenDecodeRDBLengthPrefixedVal (BS.replicate 10000000 charCodeForA)
            , bench "512MB worth of data" $ nf encodeThenDecodeRDBLengthPrefixedVal (BS.replicate 536870912 charCodeForA)
            , bench "extra large" $ nf encodeThenDecodeRDBLengthPrefixedVal (BS.replicate 1000000000 charCodeForA)
            ]
        ]

encodeThenDecodeRDBLengthPrefixedVal :: ByteString -> RDBLengthPrefixedVal
encodeThenDecodeRDBLengthPrefixedVal = fst . runGet (execSGetWithChecksum getWithChecksum) . snd . runPutM . execSPutWithChecksum . putWithChecksum . toRDBLengthPrefixedVal
