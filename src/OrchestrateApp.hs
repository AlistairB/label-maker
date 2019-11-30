module OrchestrateApp
  ( orchestrateApp
  ) where

import           Data.Text (Text, pack)
import           Polysemy
import           Polysemy.Error
import           Polysemy.Output
import           Polysemy.Reader
import           Effects
import           Types.App
import           Types.RunInput

orchestrateApp
  :: Members
    '[ Reader RunSettings
     , ReadRawLabelConfig
     , DecodeInputData
     , ProduceUpdatePlans
     , FetchOrgRepos
     , UpdateLabels
     , Error AppError
     , Output Text
     ]
    r
  => Sem r ()
orchestrateApp = do
  rawConfig <- performRead
  output $ "Raw Config: " <> (pack . show $ rawConfig)
  config <- performDecode rawConfig
  output $ "Decoded Config: " <> (pack . show $ config)
  orgRepos <- performFetchOrgRepos config
  output $ "Fetched Orgs: " <> (pack . show $ orgRepos)
  lmup <- produceUpdatePlans config orgRepos
  output $ "Produced Plans: " <> (pack . show $ lmup)
  performLabelUpdate lmup
