module RepoUpdatePlanConverter
  ( convertToPlan
  ) where

import Data.Text (Text)
import Data.Functor ((<&>))
import Data.Maybe
import Data.List

import Types.LabelConfig
import Types.FetchedData
import Types.RepoUpdatePlan

convertToPlan :: LabelMakerConfig -> FetchedAllData -> LabelMakerUpdatePlan
convertToPlan conf fetchedData = LabelMakerUpdatePlan $ concatMap (orgLabelUpdates conf) (_fetchedOrgs fetchedData)

orgLabelUpdates :: LabelMakerConfig -> FetchedOrganization -> [LabelUpdate]
orgLabelUpdates conf org = concatMap (foo conf (_fetchedOrgName org)) (_fetchedRepos org)

foo :: LabelMakerConfig -> FetchedOrgName -> FetchedRepo -> [LabelUpdate]
foo (LabelMakerConfig _ (LabelGroups syncLabels deleteLabels renameLabels)) orgName repo =
  let
    delete' = mapMaybe (doDelete orgName repo) deleteLabels
    rename = mapMaybe (doRename orgName repo syncLabels) renameLabels
    toRenameLabelNames = rename <&> _luTargetLabelName
    sync = mapMaybe (doSync orgName repo toRenameLabelNames) syncLabels
  in
    delete' ++ rename ++ sync

doDelete :: FetchedOrgName -> FetchedRepo -> DeleteLabel -> Maybe LabelUpdate
doDelete orgName (FetchedRepo repoName labels) (DeleteLabel (LabelName delLabelName)) =
  let
    labelNames = fmap _fetchedRepoLabelName labels
    shouldUpdate = elem delLabelName labelNames
  in if   shouldUpdate
     then Just $ LabelUpdate (unfetchedOrgName orgName) repoName delLabelName DeleteAction
     else Nothing

doSync :: FetchedOrgName -> FetchedRepo -> [Text] -> SyncLabel -> Maybe LabelUpdate
doSync orgName (FetchedRepo repoName labels) beingRenamed (SyncLabel (LabelName toSyncLabelName) (LabelColour toSyncLabelColour))
  | isBeingRenamed = Nothing
  | isBeingCreated = Just $ LabelUpdate
                            (unfetchedOrgName orgName)
                            repoName
                            toSyncLabelName
                            (CreateAction toSyncLabelColour)
  | isBeingUpdated = Just $ LabelUpdate
                            (unfetchedOrgName orgName)
                            repoName
                            toSyncLabelName
                            (UpdateAction toSyncLabelName toSyncLabelColour)
  | otherwise      = Nothing
    where
      isBeingRenamed = toSyncLabelName `elem` beingRenamed
      isBeingCreated = isNothing $ find (\(FetchedRepoLabel flName _) -> flName == toSyncLabelName) labels
      isBeingUpdated = isJust $ find (\(FetchedRepoLabel flName flColour) -> flName == toSyncLabelName && flColour /= toSyncLabelColour) labels

doRename :: FetchedOrgName -> FetchedRepo -> [SyncLabel] -> RenameLabel -> Maybe LabelUpdate
doRename orgName (FetchedRepo repoName labels) syncLabels (RenameLabel (LabelName oldLabelName) (LabelName newLabelName)) =
  let
    repoLabelNames = fmap _fetchedRepoLabelName labels
    shouldUpdate = elem oldLabelName repoLabelNames
  in if   shouldUpdate
     then getColourFromLabels syncLabels newLabelName <&> \labelColour ->
                LabelUpdate
                  (unfetchedOrgName orgName)
                  repoName
                  newLabelName
                  (UpdateAction oldLabelName labelColour)
     else Nothing

-- TODO: Update to have a validation step where we prove config is valid to remove cases like the new label is missing from sync
getColourFromLabels :: [SyncLabel] -> Text -> Maybe Text
getColourFromLabels syncLabels matchLabelName =
  let maybeLabel = find (\(SyncLabel (LabelName name) _) -> name == matchLabelName) syncLabels
  in  maybeLabel <&> (unLabelColour . _syncLabelColor)
