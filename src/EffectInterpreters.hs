module EffectInterpreters
 ( readRawLabelConfig
 , decodeInputData
 , EffectInterpreters.performFetchOrgRepos
 , EffectInterpreters.produceUpdatePlans
 , updateRepo
 ) where

import Data.Text (pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Bifunctor (first)
import Data.Yaml (decodeEither')
import Data.Functor ((<&>))
-- import Polysemy
-- import Polysemy.Error
-- import Polysemy.Reader

import Control.Algebra
import Control.Carrier.Reader
import Control.Carrier.Error

import Types.App (AppError(..))
import Types.LabelConfig
import Types.RunInput
import Effects
import GitHub.OrgRepoFetcher
import GitHub.OrgRepoUpdater
import RepoUpdatePlanConverter (convertToPlan)



newtype ReadRawLabelConfigIOC m a = ReadRawLabelConfigIOC { runReadRawLabelConfigIOC: m a }

-- instance (Algebra sig m, MonadIO m) => Algebra (Teletype :+: sig) (TeletypeIOC m) where
--   alg (L (Read    k)) = TeletypeIOC (liftIO getLine      >>= runTeletypeIOC . k)
--   alg (L (Write s k)) = TeletypeIOC (liftIO (putStrLn s) >>  runTeletypeIOC   k)
--   alg (R other)       = TeletypeIOC (alg (handleCoercible other))

-- readRawLabelConfig ::
--      Members [Embed IO, Reader RunSettings] r =>
--      Sem (ReadRawLabelConfig ': r) a
--   -> Sem r a
-- readRawLabelConfig =
--   interpret $ \PerformRead -> do
--     configFile <- ask <&> _configFile
--     embed $ readFile (unpack configFile) <&> (RawLabelConfig . pack)

decodeInputData ::
     Member (Error AppError) r =>
     Sem (DecodeInputData ': r) a
  -> Sem r a
decodeInputData =
  interpret $ \(PerformDecode (RawLabelConfig rawText)) ->
                fromEither $ first (const ParseFailure) $ decodeEither' $ encodeUtf8 rawText

performFetchOrgRepos ::
     Members '[Embed IO, Error AppError, Reader RunSettings] r =>
     Sem (FetchOrgRepos ': r) a
  -> Sem r a
performFetchOrgRepos =
  interpret $ \(PerformFetchOrgRepos (LabelMakerConfig orgs _)) -> do
    (RunSettings _ apiHost apiToken) <- ask
    result <- embed $ getFetchedData apiHost apiToken orgs
    fromEither result

produceUpdatePlans :: Sem (ProduceUpdatePlans ': r) a
    -> Sem r a
produceUpdatePlans =
  interpret $ \(ProduceUpdatePlans config orgs) -> pure (convertToPlan config orgs)

updateRepo ::
      Members '[Embed IO, Error AppError, Reader RunSettings] r =>
      Sem (UpdateLabels ': r) a -> Sem r a
updateRepo =
  interpret $ \(PerformLabelUpdate plan) -> do
    (RunSettings _ apiHost apiToken) <- ask
    result <- embed $ updateLabels apiHost apiToken plan
    fromEither result
