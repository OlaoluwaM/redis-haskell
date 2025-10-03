module RDBBinary (benchmarks) where

import Criterion.Main
import Redis.RDB.Binary
import Redis.RDB.Data

import Data.ByteString qualified as BS

import Data.Binary.Get (runGet)
import Data.Binary.Put (runPutM)
import Data.ByteString (ByteString)
import Data.Char (ord)
import Redis.RDB.Config (defaultRDBConfig)

benchmarks :: [Benchmark]
benchmarks =
    let charCodeForA = fromIntegral $ ord 'a'
     in [ bgroup
            "RDBBinary on roundtrip RDB length-prefixed values "
            [ bench "small" $ nf encodeThenDecodeRDBVal (BS.replicate 100 charCodeForA)
            , bench "medium" $ nf encodeThenDecodeRDBVal (BS.replicate 100000 charCodeForA)
            , bench "large" $ nf encodeThenDecodeRDBVal (BS.replicate 10000000 charCodeForA)
            , bench "512MB worth of data" $ nf encodeThenDecodeRDBVal (BS.replicate 536870912 charCodeForA)
            , bench "extra large" $ nf encodeThenDecodeRDBVal (BS.replicate 1000000000 charCodeForA)
            ]
        ]

encodeThenDecodeRDBVal :: ByteString -> RDBString
encodeThenDecodeRDBVal = fst . runGet (execRDBGet defaultRDBConfig rdbGet) . snd . runPutM . execRDBPut . rdbPut . toRDBString
