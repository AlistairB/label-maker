module OrchestrateApp
  ( orchestrateApp,
  )
where

import Control.Algebra (Has)
import Control.Effect.Trace (Trace, trace)
import Effects
import Types.Loggable

orchestrateApp ::
  ( Has ReadRawLabelConfig sig m,
    Has DecodeInputData sig m,
    Has ProduceUpdatePlans sig m,
    Has FetchOrgRepos sig m,
    Has UpdateLabels sig m,
    Has Trace sig m
  ) =>
  m ()
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
