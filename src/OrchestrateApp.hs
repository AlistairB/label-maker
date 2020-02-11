module OrchestrateApp
  ( orchestrateApp,
  )
where

import Control.Algebra (Has)
import Control.Effect.Error (Error)
import Control.Effect.Trace (Trace)
import Effects
import Types.App
import Types.Loggable
import Types.RunInput

--  Members
--     '[ Reader RunSettings
--      , ReadRawLabelConfig
--      , DecodeInputData
--      , ProduceUpdatePlans
--      , FetchOrgRepos
--      , UpdateLabels
--      , Error AppError
--      , Trace
--      ]
--     r

orchestrateApp ::
  ( Has ReadRawLabelConfig sig m,
    Has DecodeInputData sig m,
    Has ProduceUpdatePlans sig m,
    Has FetchOrgRepos sig m,
    Has UpdateLabels sig m,
    Has (Error AppError) sig m,
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
