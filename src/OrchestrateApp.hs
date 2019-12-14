module OrchestrateApp
  ( orchestrateApp
  ) where

import           Polysemy
import           Polysemy.Error
import           Polysemy.Trace
import           Polysemy.Reader
import           Data.Text (unpack)
import           Data.List (intercalate)

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
  trace $ "Fetched Orgs: " <> show orgRepos
  lmup <- produceUpdatePlans config orgRepos
  trace $ "Produced Plans: " <> show lmup
  performLabelUpdate lmup

toLog :: Loggable a => a -> String
toLog = intercalate "\n" . fmap unpack . getLogs
