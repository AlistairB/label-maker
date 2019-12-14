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
import           Types.Loggable

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
  trace $ "Raw Config: " <> toLog rawConfig
  config <- performDecode rawConfig
  trace $ "Decoded Config: " <> toLog config
  orgRepos <- performFetchOrgRepos config
  trace $ "Fetched Orgs: " <> toLog orgRepos
  lmup <- produceUpdatePlans config orgRepos
  trace $ "Produced Plans: " <> toLog lmup
  performLabelUpdate lmup
