{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Effects where

import Control.Effect.Class (HFunctor, Effect)
import Control.Algebra (Has, send)
import GHC.Generics (Generic1)

import Types.LabelConfig
import Types.RepoUpdatePlan
import Types.FetchedData

newtype ReadRawLabelConfig m k
  = PerformRead (RawLabelConfig -> m k)
  deriving (Functor, Generic1)

instance HFunctor ReadRawLabelConfig
instance Effect   ReadRawLabelConfig

performRead :: Has ReadRawLabelConfig sig m => m RawLabelConfig
performRead = send (PerformRead pure)

-- data ReadRawLabelConfig m a where
--   PerformRead :: ReadRawLabelConfig m RawLabelConfig

data DecodeInputData m k
  = PerformDecode RawLabelConfig (LabelMakerConfig -> m k)
  deriving (Functor, Generic1)

instance HFunctor DecodeInputData
instance Effect   DecodeInputData

performDecode :: Has DecodeInputData sig m => RawLabelConfig -> m LabelMakerConfig
performDecode rawLabelConfig = send (PerformDecode rawLabelConfig pure)


-- data DecodeInputData m a where
--   PerformDecode :: RawLabelConfig -> DecodeInputData m LabelMakerConfig

data FetchOrgRepos m k
  = PerformFetchOrgRepos LabelMakerConfig (FetchedAllData -> m k)
  deriving (Functor, Generic1)

instance HFunctor FetchOrgRepos
instance Effect   FetchOrgRepos

performFetchOrgRepos :: Has FetchOrgRepos sig m => LabelMakerConfig -> m FetchedAllData
performFetchOrgRepos labelMakerConfig = send (PerformFetchOrgRepos labelMakerConfig pure)

-- data FetchOrgRepos m a where
--   PerformFetchOrgRepos :: LabelMakerConfig -> FetchOrgRepos m FetchedAllData

data ProduceUpdatePlans m k
  = ProduceUpdatePlans LabelMakerConfig FetchedAllData (LabelMakerUpdatePlan -> m k)
  deriving (Functor, Generic1)

instance HFunctor ProduceUpdatePlans
instance Effect   ProduceUpdatePlans

produceUpdatePlans :: Has ProduceUpdatePlans sig m => LabelMakerConfig -> FetchedAllData -> m LabelMakerUpdatePlan
produceUpdatePlans labelMakerConfig fetchedAllData = send (ProduceUpdatePlans labelMakerConfig fetchedAllData pure)

-- data ProduceUpdatePlans m a where
--   ProduceUpdatePlans :: LabelMakerConfig -> FetchedAllData -> ProduceUpdatePlans m LabelMakerUpdatePlan

data UpdateLabels m k
  = PerformLabelUpdate LabelMakerUpdatePlan (m k)
  deriving (Functor, Generic1)

performLabelUpdate :: (Has UpdateLabels sig m) => LabelMakerUpdatePlan -> m ()
performLabelUpdate labelMakerUpdatePlan = send (PerformLabelUpdate labelMakerUpdatePlan (pure ()))

instance HFunctor UpdateLabels
instance Effect   UpdateLabels

-- data UpdateLabels m a where
--   PerformLabelUpdate :: LabelMakerUpdatePlan -> UpdateLabels m ()

-- makeSem ''ReadRawLabelConfig
-- makeSem ''DecodeInputData
-- makeSem ''ProduceUpdatePlans
-- makeSem ''FetchOrgRepos
-- makeSem ''UpdateLabels
