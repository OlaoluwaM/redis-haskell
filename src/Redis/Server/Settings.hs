module Redis.Server.Settings (
  Settings (..)
) where

import Data.Default (Default (..))
import Data.Text (Text)

data Settings = Settings {rdbDir :: Maybe Text, rdbFilename :: Maybe Text}

instance Default Settings where
  def = Settings Nothing Nothing

