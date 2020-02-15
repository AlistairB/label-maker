{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module EffectInterpreters
  ( ReadRawLabelConfigIOC (..),
    DecodeInputDataC (..),
    FetchOrgReposIOC (..),
    ProduceUpdatePlansC (..),
    UpdateLabelsIOC (..),
  )
where

import Control.Algebra
import Control.Carrier.Error.Either
import Control.Carrier.Reader
import Control.Effect.Exception (catch, SomeException)
import Control.Effect.Lift
import Control.Monad.IO.Class
import Data.Bifunctor (first)
import Data.Functor ((<&>))
import Data.Text (pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (decodeEither')
import Effects
import GitHub.OrgRepoFetcher
import GitHub.OrgRepoUpdater
import RepoUpdatePlanConverter (convertToPlan)
import Types.App (AppError (..))
import Types.LabelConfig
import Types.RunInput

newtype ReadRawLabelConfigIOC m a = ReadRawLabelConfigIOC {runReadRawLabelConfigIOC :: m a}
  deriving (Applicative, Functor, Monad, MonadIO)

instance
  (Algebra sig m, Has (Reader RunSettings) sig m, Has (Error AppError) sig m, Has (Lift IO) sig m, MonadIO m) =>
  Algebra (ReadRawLabelConfig :+: sig) (ReadRawLabelConfigIOC m)
  where
  alg (L (PerformRead k)) =
    ( do
        configFile <- ask <&> _configFile
        let ioFile = readFile (unpack configFile) <&> (RawLabelConfig . pack)
        catch (liftIO ioFile) exceptionToFusedError
    )
      >>= k
  alg (R other) = ReadRawLabelConfigIOC (alg (handleCoercible other))

exceptionToFusedError ::
  (Has (Error AppError) sig m
  ) => SomeException -> m a
exceptionToFusedError _ = throwError ReadFileException

newtype DecodeInputDataC m a = DecodeInputDataC {runDecodeInputDataC :: m a}
  deriving (Applicative, Functor, Monad, MonadIO) -- seems MonadIO is needed, else you can't chain this with IO interpreters

fromEither :: (Has (Throw l) sig m) => Either l r -> m r
fromEither (Left e) = throwError e
fromEither (Right a) = pure a

instance (Algebra sig m, Has (Error AppError) sig m) => Algebra (DecodeInputData :+: sig) (DecodeInputDataC m) where
  alg (L (PerformDecode (RawLabelConfig rawText) k)) =
    (fromEither . first (const ParseFailure) . decodeEither' . encodeUtf8) rawText >>= k
  alg (R other) = DecodeInputDataC (alg (handleCoercible other))

newtype FetchOrgReposIOC m a = FetchOrgReposIOC {runFetchOrgReposIOC :: m a}
  deriving (Applicative, Functor, Monad, MonadIO)

instance
  ( Algebra sig m,
    Has (Reader RunSettings) sig m,
    Has (Error AppError) sig m,
    MonadIO m
  ) =>
  Algebra (FetchOrgRepos :+: sig) (FetchOrgReposIOC m)
  where
  alg (L (PerformFetchOrgRepos (LabelMakerConfig orgs _) k)) = k =<< do
    (RunSettings _ apiHost apiToken) <- ask
    result <- liftIO $ getFetchedData apiHost apiToken orgs
    fromEither result
  alg (R other) = FetchOrgReposIOC (alg (handleCoercible other))

newtype ProduceUpdatePlansC m a = ProduceUpdatePlansC {runProduceUpdatePlansC :: m a}
  deriving (Applicative, Functor, Monad, MonadIO)

instance (Algebra sig m) => Algebra (ProduceUpdatePlans :+: sig) (ProduceUpdatePlansC m) where
  alg (L (ProduceUpdatePlans config allData k)) = pure (convertToPlan config allData) >>= k
  alg (R other) = ProduceUpdatePlansC (alg (handleCoercible other))

newtype UpdateLabelsIOC m a = UpdateLabelsIOC {runUpdateLabelsIOC :: m a}
  deriving (Applicative, Functor, Monad, MonadIO)

instance (Algebra sig m, Has (Error AppError) sig m, Has (Reader RunSettings) sig m, MonadIO m) => Algebra (UpdateLabels :+: sig) (UpdateLabelsIOC m) where
  alg (L (PerformLabelUpdate plan k)) =
    ( do
        (RunSettings _ apiHost apiToken) <- ask
        result <- liftIO $ updateLabels apiHost apiToken plan
        fromEither result
    )
      >> k
  alg (R other) = UpdateLabelsIOC (alg (handleCoercible other))
