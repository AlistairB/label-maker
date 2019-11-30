module Lib
    ( runApp
    ) where

import Polysemy
import Polysemy.Output
import Polysemy.Error
import Polysemy.Reader
import EffectInterpreters
import Types.RunInput
import Data.Foldable (traverse_)
import OrchestrateApp

runApp :: RunSettings -> IO ()
runApp runSettings = do
    (logs, result) <-
      runM $ 
        ( runOutputList
        . runError
        . runReader runSettings
        . updateRepo
        . produceUpdatePlans
        . performFetchOrgRepos
        . decodeInputData
        . readRawLabelConfig
        ) orchestrateApp
    traverse_ print logs
    case result of
      (Left  appError   ) -> putStrLn $ "Failed with: " <> show appError
      (Right finalResult) -> putStrLn $ "Completed with " <> show finalResult
