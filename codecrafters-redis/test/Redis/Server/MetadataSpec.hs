{-# OPTIONS_GHC -Wno-x-unsafe-internals #-}

module Redis.Server.MetadataSpec where

import Redis.Server.Metadata

import Data.Text qualified as T
import Effectful qualified as Eff
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Seed qualified as Seed
import Hedgehog.Range qualified as Range

import Data.Char (isAlpha, isHexDigit, isLowerCase)
import Redis.Effect.Random (runRandomPure)
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)

test_replId_generator :: [TestTree]
test_replId_generator =
    [ testProperty "Generated replication Ids are valid per upstream" genValidReplId
    ]

genValidReplId :: H.Property
genValidReplId = H.property $ do
    seed <- H.forAll (Seed.from <$> Gen.word64 Range.linearBounded)
    let generatedReplId = Eff.runPureEff . runRandomPure seed $ genReplId_
    let shouldBeOfExpectedSize = T.length generatedReplId == replIdLength

    -- Per the rules of logic "if p then q" is equivalent to "not p or q". So "if isAlpha, then check isLowerCase" is equivalent to "not isAlpha || isLowerCase"
    let condFn c = isHexDigit c && (not (isAlpha c) || isLowerCase c)
    let shouldBeMadeUpOfOnlyHexDigits = T.all condFn generatedReplId

    H.assert shouldBeOfExpectedSize
    H.assert shouldBeMadeUpOfOnlyHexDigits
