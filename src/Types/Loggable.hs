{-# LANGUAGE FlexibleInstances #-}
module Types.Loggable
  (Loggable(..)) where

import Data.Text (Text, pack)

import Types.LabelConfig
import Types.FetchedData

class Loggable a where
  getLogs :: a -> [Text]

instance (Loggable a, Loggable b) => Loggable (Either a b) where
  getLogs (Right a) = "Successful!" : getLogs a
  getLogs (Left e) = "Failure!" : getLogs e

instance Loggable RawLabelConfig where
  getLogs a = [ unRawLabelConfig a ]

instance Loggable LabelMakerConfig where
  getLogs _ = [ "..." ]

instance Loggable FetchedAllData where
  getLogs = concatMap getLogs . _fetchedOrgs

instance Loggable FetchedOrganization where
  getLogs (FetchedOrganization (FetchedOrgName orgName) repos) =
    ("Fetched organization: " <> orgName) : concatMap getLogs repos

instance Loggable FetchedRepo where
  getLogs (FetchedRepo repoName repoLabels) =
    [ "Fetched repo " <> repoName <> " with " <> (pack . show $ length repoLabels) <> " labels" ]
