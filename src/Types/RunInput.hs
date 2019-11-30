module Types.RunInput
  ( RunSettings(..)
  ) where

import Data.Text (Text)

data RunSettings = RunSettings
  { _configFile :: Text
  , _githubApiHost :: Text
  , _githubApiKey :: Text
  } deriving (Eq, Show)
