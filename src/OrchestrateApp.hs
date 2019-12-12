module OrchestrateApp
  ( orchestrateApp
  ) where

import           Polysemy
import           Polysemy.Error
import           Polysemy.Trace
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
     , Trace
     ]
    r
  => Sem r ()
orchestrateApp = do
  rawConfig <- performRead
  trace $ "Raw Config: " <> show rawConfig
  config <- performDecode rawConfig
  trace $ "Decoded Config: " <> show config
  orgRepos <- performFetchOrgRepos config
  trace $ "Fetched Orgs: " <> show orgRepos
  lmup <- produceUpdatePlans config orgRepos
  trace $ "Produced Plans: " <> show lmup
  performLabelUpdate lmup
