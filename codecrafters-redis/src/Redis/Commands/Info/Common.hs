module Redis.Commands.Info.Common (
    formatToInfoSectionField,
    maskFieldWhenNecessary,
) where

import Data.String (IsString)
import Redis.Server.Metadata (Environment (..))

formatToInfoSectionField :: (Semigroup a, IsString a) => a -> a -> a
formatToInfoSectionField name value = name <> ":" <> value <> "\r\n"

-- Useful for test environment where field values with inherent entropy (with some randomization to them) can throw off golden tests unnecessarily
maskFieldWhenNecessary :: (IsString a) => Environment -> a -> a
maskFieldWhenNecessary DEV a = a
maskFieldWhenNecessary PROD a = a
maskFieldWhenNecessary TEST _ = "<MASKED>"
