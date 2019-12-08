{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
module LabelConfigSpec
  ( spec
  ) where

import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Yaml
import Test.Hspec
import Text.RawString.QQ
import Data.ByteString (ByteString)

import Types.LabelConfig


testYAML :: ByteString
testYAML = [r|---
organizations:
  - cool-org
  - other-org:
      repos:
        - thinger
        - wrangler

labels:
  sync:
    awesome-issue: '000000'
    wont-fix: b98fe0
  delete:
    - bad-issue-label
    - straight-to-prod
  rename:
    - old-label-name: will-not-fix
      new-label-name: wont-fix|]


justSync :: ByteString
justSync = [r|---
organizations:
  - cool-org
  - other-org:
      repos:
        - thinger
        - wrangler
labels:
  sync:
    awesome-issue: '000000'
    wont-fix: b98fe0
|]

repos :: ByteString
repos = "[ \"thinger\", \"wrangler\" ]"

reposAll :: ByteString
reposAll = "all"

deleteLabelYAML :: ByteString
deleteLabelYAML = "bad-issue-label"

labelNameYAML :: ByteString
labelNameYAML = "thinger"

organizationRepoYAML :: ByteString
organizationRepoYAML = "wrangler"

spec :: Spec
spec =
  describe "FromJSON" $ do
    it "decodes a DeleteLabel" $ do
      let result = decodeEither deleteLabelYAML :: Either String DeleteLabel
          expected = Right $ DeleteLabel (LabelName "bad-issue-label")

      result `shouldBe` expected

    it "decodes a LabelName" $ do
      let result = decodeEither labelNameYAML :: Either String LabelName
          expected = Right $ LabelName "thinger"

      result `shouldBe` expected

    it "decodes an OrganizationRepo" $ do
      let result = decodeEither organizationRepoYAML :: Either String OrganizationRepo
          expected = Right $ OrganizationRepo "wrangler"

      result `shouldBe` expected

    it "decodes the LabelMakerConfig" $ do
      let result = decodeEither testYAML
          organizations = Organization "cool-org" OrganizationReposAll
            :| [Organization "other-org" $ OrganizationReposSpecific $ OrganizationRepo
                <$> ("thinger" :| ["wrangler"])]
          sync =
            [ SyncLabel (LabelName "wont-fix") (LabelColour "b98fe0")
            , SyncLabel (LabelName "awesome-issue") (LabelColour "000000")
            ]
          delete =
            [ DeleteLabel (LabelName "bad-issue-label")
            , DeleteLabel (LabelName "straight-to-prod")
            ]
          rename =
            [ RenameLabel (LabelName "will-not-fix") (LabelName "wont-fix")
            ]
          expected = Right $ LabelMakerConfig organizations (LabelGroups sync delete rename)
      result `shouldBe` expected

    it "decodes the LabelMakerConfig with just sync" $ do
      let result = decodeEither justSync
          organizations = Organization "cool-org" OrganizationReposAll
            :| [Organization "other-org" $ OrganizationReposSpecific $ OrganizationRepo
                <$> ("thinger" :| ["wrangler"])]
          sync =
            [ SyncLabel (LabelName "wont-fix") (LabelColour "b98fe0")
            , SyncLabel (LabelName "awesome-issue") (LabelColour "000000")
            ]
          delete = []
          rename = []
          expected = Right $ LabelMakerConfig organizations (LabelGroups sync delete rename)
      result `shouldBe` expected

    it "decodes the org repos" $ do
      let result = decodeEither repos
          expected = Right $ OrganizationReposSpecific $ OrganizationRepo <$> ("thinger" :| ["wrangler"])
      result `shouldBe` expected

    it "decodes the org all repos" $ do
      let result = decodeEither reposAll
          expected = Right  OrganizationReposAll
      result `shouldBe` expected
