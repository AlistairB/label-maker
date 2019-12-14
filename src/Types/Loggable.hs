{-# LANGUAGE FlexibleInstances #-}
module Types.Loggable
  ( Loggable(..)
  , toLog
  ) where

import Data.Text (Text, pack, unpack)
import Data.List (intercalate)

import qualified Data.List.NonEmpty as NE

import Types.LabelConfig
import Types.FetchedData
import Types.RepoUpdatePlan
import Types.App


class Loggable a where
  getLogs :: a -> [Text]

toLog :: Loggable a => a -> String
toLog = intercalate "\n" . fmap unpack . getLogs

instance Loggable () where
  getLogs _ = [ "no errors" ]

instance Loggable AppError where
  getLogs ParseFailure = [ "Failed to parse the config" ]
  getLogs (GithubErrors errors) = "Github API Errors!" : concatMap getLogs (NE.toList errors)

instance Loggable SpecificGithubError where
  getLogs (SGFetchOrgReposError m) = [ "Failed to fetch organization: " <> m ]
  getLogs (SGFetchOrgRepoError m) = [ "Failed to fetch repo: " <> m ]
  getLogs (SGFetchRepoLabelError m) = [ "Failed to fetch repo labels: " <> m ]
  getLogs (SGLabelWriteError m) = [ "Failed to perform label update: " <> m ]

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

instance Loggable LabelMakerUpdatePlan where
  getLogs = concatMap getLogs . unLabelMakerUpdatePlan

instance Loggable LabelUpdate where
  getLogs (LabelUpdate orgName repoName targetLabelName labelAction) =
    let genericMessage = "Perform update on org " <> orgName <> " > repo " <> repoName <> " > label " <> targetLabelName <> " : "
        specificMessage = case labelAction of
                            DeleteAction -> "Delete"
                            (CreateAction colour) -> "Create with color " <> colour
                            (UpdateAction oldName newColour) -> "Rename from " <> oldName <> " with color " <> newColour
    in  [ genericMessage <> specificMessage ]
