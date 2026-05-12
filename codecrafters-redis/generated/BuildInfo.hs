module BuildInfo (gitSha1, gitDirty, ghcVersion, buildId) where

import Data.String (IsString)

gitSha1 :: (IsString a) => a
gitSha1 = "86dce54a"

gitDirty :: Bool
gitDirty = True

ghcVersion :: (IsString a) => a
ghcVersion = "9.8.4"

buildId :: (IsString a) => a
buildId = "12690652462249670469"
