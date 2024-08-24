module Utils (myTraceM, myTrace, AlphabeticString (..), pass) where

import Data.String (IsString (fromString))
import Test.QuickCheck
import "hs-redis-clone" Utils (myTrace, myTraceM)
import Test.Hspec (shouldBe, Expectation)

newtype AlphaChar = AlphaChar {unAlphaChar :: Char} deriving (Eq, Show)

genAlphaChar :: Gen AlphaChar
genAlphaChar = do
    let lowerCaseChar = AlphaChar <$> elements ['a' .. 'z']
    let upperCaseChar = AlphaChar <$> elements ['A' .. 'Z']
    oneof [lowerCaseChar, upperCaseChar]

instance Arbitrary AlphaChar where
    arbitrary = genAlphaChar

newtype AlphabeticString a = AlphabeticString {unAlphabeticString :: a} deriving (Eq, Show, Ord)

instance (IsString a) => Arbitrary (AlphabeticString a) where
    arbitrary = AlphabeticString . fromString <$> listOf ((.unAlphaChar) <$> genAlphaChar)

pass :: Expectation
pass = True `shouldBe` True
