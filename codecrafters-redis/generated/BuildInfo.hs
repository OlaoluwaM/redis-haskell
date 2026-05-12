module BuildInfo (gitSha1, gitDirty, buildId) where

import Data.String (IsString)

gitSha1 :: (IsString a) => a
gitSha1 = "eba89f68"

gitDirty :: Bool
gitDirty = True

buildId :: (IsString a) => a
buildId = "6392652505867996143"
