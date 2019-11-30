module RepoUpdatePlanConverterSpec
 ( spec
 ) where

import Data.List.NonEmpty (NonEmpty((:|)))
import Test.Hspec

import Types.RepoUpdatePlan
import Types.LabelConfig
import Types.FetchedData
import RepoUpdatePlanConverter (convertToPlan)

fullConfig :: LabelMakerConfig
fullConfig = LabelMakerConfig {
                _organizations = Organization {_orgName = "scala-course", _orgRepos = OrganizationReposAll }
                                :| [Organization {_orgName = "locations", _orgRepos = OrganizationReposSpecific ( OrganizationRepo "awesome-app" :| []) }],
                                _labelGroups = LabelGroups {
                                    _sync = [
                                        SyncLabel {_syncLabelName = LabelName "renamed", _syncLabelColor = LabelColour "000000"},
                                        SyncLabel {_syncLabelName = LabelName "update-colour", _syncLabelColor = LabelColour "333333"},
                                        SyncLabel {_syncLabelName = LabelName "new-label", _syncLabelColor = LabelColour "b98fe0"}
                                    ],
                                    _delete = [DeleteLabel (LabelName "to-delete")],
                                    _rename = [RenameLabel {_renameLabelFrom = LabelName "to-rename", _renameLabelTo = LabelName "renamed"}]}}

fetchedAllData :: FetchedAllData
fetchedAllData = FetchedAllData [
                FetchedOrganization {_fetchedOrgName = FetchedOrgName {unfetchedOrgName = "scala-course"},
                    _fetchedRepos = [
                        FetchedRepo {
                            _fetchedRepoName = "applied-scala",
                            _fetchedRepoLabels =
                                [
                                    FetchedRepoLabel {_fetchedRepoLabelName = "to-delete", _fetchedRepoLabelColour = "d73a4a"},
                                    FetchedRepoLabel {_fetchedRepoLabelName = "to-rename", _fetchedRepoLabelColour = "cfd3d7"},
                                    FetchedRepoLabel {_fetchedRepoLabelName = "update-colour", _fetchedRepoLabelColour = "a2eeef"}
                                ]
                            }
                    ]
                 },
                 FetchedOrganization {_fetchedOrgName = FetchedOrgName {unfetchedOrgName = "locations"},
                 _fetchedRepos = [
                     FetchedRepo {
                         _fetchedRepoName = "awesome-app",
                         _fetchedRepoLabels =
                             [
                                 FetchedRepoLabel {_fetchedRepoLabelName = "to-delete", _fetchedRepoLabelColour = "d73a4a"},
                                 FetchedRepoLabel {_fetchedRepoLabelName = "to-rename", _fetchedRepoLabelColour = "cfd3d7"},
                                 FetchedRepoLabel {_fetchedRepoLabelName = "update-colour", _fetchedRepoLabelColour = "a2eeef"}
                             ]
                         }
                 ]
              }
              ]


spec :: Spec
spec =
  describe "convertToPlan" $
    it "converts a big plan" $
      let
          getActions orgName repoName =
            [
                LabelUpdate {_luOrgName = orgName, _luRepoName = repoName, _luTargetLabelName = "to-delete", _luLabelAction = DeleteAction},
                LabelUpdate {_luOrgName = orgName, _luRepoName = repoName, _luTargetLabelName = "renamed", _luLabelAction = UpdateAction "to-rename" "000000"},
                LabelUpdate {_luOrgName = orgName, _luRepoName = repoName, _luTargetLabelName = "update-colour", _luLabelAction = UpdateAction "update-colour" "333333"},
                LabelUpdate {_luOrgName = orgName, _luRepoName = repoName, _luTargetLabelName = "new-label", _luLabelAction = CreateAction "b98fe0"}
            ]
          expected = LabelMakerUpdatePlan $
                           getActions "scala-course" "applied-scala"
                        ++ getActions "locations" "awesome-app"

          result = convertToPlan fullConfig fetchedAllData

      in result `shouldBe` expected
