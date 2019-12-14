module Lib
    ( runApp
    ) where

import Polysemy
import Polysemy.Trace
import Polysemy.Error
import Polysemy.Reader
import EffectInterpreters
import Types.RunInput
import Types.Loggable (toLog)
import OrchestrateApp

runApp :: RunSettings -> IO ()
runApp runSettings = do
    result <-
      runM $
        ( traceToIO
        . runError
        . runReader runSettings
        . updateRepo
        . produceUpdatePlans
        . performFetchOrgRepos
        . decodeInputData
        . readRawLabelConfig
        ) orchestrateApp
    case result of
      (Left  appError   ) -> putStrLn $ "Failed with: " <> toLog appError
      (Right finalResult) -> putStrLn $ "Completed with " <> toLog finalResult
