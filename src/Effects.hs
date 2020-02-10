{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Effects where

import Control.Effect.Class (HFunctor, Effect)
import GHC.Generics (Generic1)

import Types.LabelConfig
import Types.RepoUpdatePlan
import Types.FetchedData

newtype ReadRawLabelConfig m k
  = PerformRead (String -> m k)
  deriving (Functor, Generic1)

instance HFunctor ReadRawLabelConfig
instance Effect   ReadRawLabelConfig

-- data ReadRawLabelConfig m a where
--   PerformRead :: ReadRawLabelConfig m RawLabelConfig

newtype DecodeInputData m k
  = PerformDecode (RawLabelConfig -> LabelMakerConfig -> m k)
  deriving (Functor, Generic1)

instance HFunctor DecodeInputData
instance Effect   DecodeInputData

-- data DecodeInputData m a where
--   PerformDecode :: RawLabelConfig -> DecodeInputData m LabelMakerConfig

newtype FetchOrgRepos m k
  = PerformFetchOrgRepos (LabelMakerConfig -> FetchedAllData -> m k)
  deriving (Functor, Generic1)

instance HFunctor FetchOrgRepos
instance Effect   FetchOrgRepos

-- data FetchOrgRepos m a where
--   PerformFetchOrgRepos :: LabelMakerConfig -> FetchOrgRepos m FetchedAllData

newtype ProduceUpdatePlans m k
  = ProduceUpdatePlans (LabelMakerConfig -> FetchedAllData -> LabelMakerUpdatePlan -> m k)
  deriving (Functor, Generic1)

instance HFunctor ProduceUpdatePlans
instance Effect   ProduceUpdatePlans

-- data ProduceUpdatePlans m a where
--   ProduceUpdatePlans :: LabelMakerConfig -> FetchedAllData -> ProduceUpdatePlans m LabelMakerUpdatePlan

data UpdateLabels m k
  = PerformLabelUpdate LabelMakerUpdatePlan (m k)
  deriving (Functor, Generic1)

instance HFunctor UpdateLabels
instance Effect   UpdateLabels

-- data UpdateLabels m a where
--   PerformLabelUpdate :: LabelMakerUpdatePlan -> UpdateLabels m ()

-- makeSem ''ReadRawLabelConfig
-- makeSem ''DecodeInputData
-- makeSem ''ProduceUpdatePlans
-- makeSem ''FetchOrgRepos
-- makeSem ''UpdateLabels
