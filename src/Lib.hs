{-# LANGUAGE TypeApplications #-}
module Lib
    ( runApp
    ) where

-- import Polysemy
-- import Polysemy.Trace
-- import Polysemy.Error
-- import Polysemy.Reader

import Control.Carrier.Lift (runM)
import Control.Carrier.Error.Either (runError)
import Control.Carrier.Reader (runReader)
import Control.Carrier.Trace.Printing (runTrace)

import EffectInterpreters
import Types.RunInput
import Types.App
import Types.Loggable (toLog)
import OrchestrateApp

runApp :: RunSettings -> IO ()
runApp runSettings = do
    result <-
      runM $
        ( runTrace
        . runError @AppError
        . runReader @RunSettings runSettings
        . runUpdateLabelsIOC
        . runProduceUpdatePlansC
        . runFetchOrgReposIOC
        . runDecodeInputDataC
        . runReadRawLabelConfigIOC
        ) orchestrateApp
    case result of
      (Left  appError   ) -> putStrLn $ "Failed with: " <> toLog appError
      (Right finalResult) -> putStrLn $ "Completed with " <> toLog finalResult
