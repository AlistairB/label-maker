module GitHub.OrgRepoUpdater
  ( updateLabels
  )where

import qualified GitHub                          as Github
import qualified Control.Concurrent.ParallelIO.Local as Local
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Proxy
import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import Data.List.NonEmpty as NE

import Types.App (AppError(..), SpecificGithubError(..))
import Types.RepoUpdatePlan

getGheAuth :: Text -> Text -> Github.Auth
getGheAuth apiHost apiToken =
  Github.EnterpriseOAuth
    ("https://" <> apiHost <> "/api/v3")
    (encodeUtf8 apiToken)

updateLabels :: Text -> Text -> LabelMakerUpdatePlan -> IO (Either AppError ())
updateLabels apiHost apiToken (LabelMakerUpdatePlan updates) = do
  let gheAuth = getGheAuth apiHost apiToken
  results <- Local.withPool 10 $ \pool ->
    Local.parallel pool $ fmap (runLabel gheAuth) updates
  let (failures, _) = partitionEithers results
  pure $ case failures of
    []     -> Right ()
    (x:xs) -> Left (GithubErrors $ x :| xs)

runLabel :: Github.Auth -> LabelUpdate -> IO (Either SpecificGithubError ())
runLabel gheAuth (LabelUpdate orgName repoName labelName labelAction) = do
  let namedOrg = Github.mkName Proxy orgName
      namedRepo = Github.mkName Proxy repoName
      namedLabel = Github.mkName Proxy labelName
  result <- case labelAction of
    DeleteAction -> Github.github gheAuth $ Github.deleteLabelR namedOrg namedRepo namedLabel
    (CreateAction colour) -> (fmap . fmap) (const ()) $ Github.github gheAuth $ Github.createLabelR namedOrg namedRepo namedLabel (unpack colour)
    (UpdateAction oldLabelName colour) -> (fmap . fmap) (const ())
      $ Github.github gheAuth $ Github.updateLabelR namedOrg namedRepo (Github.mkName Proxy oldLabelName) namedLabel (unpack colour)
  pure $ first (SGLabelWriteError . pack . show) result
