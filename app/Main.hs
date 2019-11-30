module Main
  ( main
  ) where

import Options.Applicative
import Data.Semigroup ((<>))

import Lib
import Types.RunInput

main :: IO ()
main = runApp =<< execParser opts
  where
    opts = info (inputParser <**> helper)
      ( fullDesc
      <> progDesc "Sync labels for a number of orgs or repos"
      <> header "Label Maker - Sync Github Labels" )

inputParser :: Parser RunSettings
inputParser = RunSettings
                <$> strOption
                    ( long "config-file"
                    <> help "Config file with github label settings" )
                <*> strOption
                  ( long "github-api-host"
                  <> help "The github API hostname"
                  <> value "api.github.com" )
                <*> strOption
                  ( long "github-token"
                  <> help "A github token with repo level permissions" )
