{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}
{-# LANGUAGE TemplateHaskell #-}

module Effects where

import Polysemy

import Types.LabelConfig
import Types.RepoUpdatePlan
import Types.FetchedData

data ReadRawLabelConfig m a where
  PerformRead :: ReadRawLabelConfig m RawLabelConfig

data DecodeInputData m a where
  PerformDecode :: RawLabelConfig -> DecodeInputData m LabelMakerConfig

data FetchOrgRepos m a where
  PerformFetchOrgRepos :: LabelMakerConfig -> FetchOrgRepos m FetchedAllData

data ProduceUpdatePlans m a where
  ProduceUpdatePlans :: LabelMakerConfig -> FetchedAllData -> ProduceUpdatePlans m LabelMakerUpdatePlan

data UpdateLabels m a where
  PerformLabelUpdate :: LabelMakerUpdatePlan -> UpdateLabels m ()

makeSem ''ReadRawLabelConfig
makeSem ''DecodeInputData
makeSem ''ProduceUpdatePlans
makeSem ''FetchOrgRepos
makeSem ''UpdateLabels
