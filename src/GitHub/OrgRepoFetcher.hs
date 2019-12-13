module GitHub.OrgRepoFetcher
  ( getFetchedData
  )where

import qualified GitHub                         as Github
import qualified Control.Concurrent.ParallelIO.Local as Local
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Proxy
import Data.Vector as VEC
import Data.Functor ((<&>))
import Data.Traversable (for)
import Data.List.NonEmpty as NE
import Data.Either (partitionEithers)
import Data.Bifunctor (first)
import Control.Monad (join)

import Types.FetchedData
import Types.LabelConfig
import Types.App (AppError(..), SpecificGithubError(..))

getGheAuth :: Text -> Text -> Github.Auth
getGheAuth apiHost apiToken =
  Github.EnterpriseOAuth
    ("https://" <> apiHost <> "/api/v3")
    (encodeUtf8 apiToken)

getFetchedData :: Text -> Text -> NonEmpty Organization -> IO (Either AppError FetchedAllData)
getFetchedData apiHost apiToken orgs = do
  let gheAuth = getGheAuth apiHost apiToken
  eitherListOrgs <- for orgs (getOrgRepos gheAuth)
  let (failures, successes) = partitionEithers (NE.toList eitherListOrgs)
  pure $ case failures of
    [] -> Right $ FetchedAllData successes
    xs -> Left $ GithubErrors (doConcat xs)

-- TODO: fix this not to use NE.toList which is unsafe (and we do know the list can't be empty)
doConcat :: [NonEmpty SpecificGithubError] -> NonEmpty SpecificGithubError
doConcat xs = NE.fromList $ Prelude.concat $ xs <&> NE.toList

getOrgRepos :: Github.Auth -> Organization -> IO (Either (NonEmpty SpecificGithubError) FetchedOrganization)
getOrgRepos gheAuth (Organization orgName OrganizationReposAll) = getAllOrgRepos gheAuth orgName
getOrgRepos gheAuth (Organization orgName (OrganizationReposSpecific repoNames)) = do
  fetchedOrgResults <- for repoNames (getSpecificRepo gheAuth orgName . unOrganizationRepo)
  let (failures, successes) = partitionEithers (NE.toList fetchedOrgResults)
  pure $ case failures of
    [] -> Right $ FetchedOrganization (FetchedOrgName orgName) successes
    (x:xs) -> Left (x :| xs)

getSpecificRepo :: Github.Auth -> Text -> Text -> IO (Either SpecificGithubError FetchedRepo)
getSpecificRepo gheAuth orgName repoName = do
  let owner = Github.mkName Proxy orgName
      repo  = Github.mkName Proxy repoName
  loadedRepo <- Github.github gheAuth (Github.repositoryR owner repo)
  let errorMappedLoadedRepo = first (SGFetchOrgRepoError . pack . show) loadedRepo
  let notArchivedErrMapLoadedRepo = errorMappedLoadedRepo >>= \r -> if | Github.repoArchived r ->
                                                                            Left (SGFetchOrgRepoError $ "Specific repo org: " <> orgName <> " repo: " <> repoName <> " is archived.")
                                                                       | otherwise -> Right r

  fetchedRepo <- for notArchivedErrMapLoadedRepo (getRepoLabels gheAuth)
  pure $ first (SGFetchRepoLabelError . pack . show) (join fetchedRepo)

getAllOrgRepos :: Github.Auth -> Text -> IO (Either (NonEmpty SpecificGithubError) FetchedOrganization)
getAllOrgRepos gheAuth orgName = do
  let name      = Github.mkName Proxy orgName
      publicity = Github.RepoPublicityAll
  eitherRepos <- Github.github gheAuth (Github.organizationReposR name publicity Github.FetchAll)
  let eitherListRepos = eitherRepos <&> VEC.toList
  case eitherListRepos of
    Right repos ->
      let nonArchivedRepos = Prelude.filter (not . Github.repoArchived) repos
      in  getFetchedOrganisation gheAuth orgName nonArchivedRepos
    Left e -> pure $ Left $ SGFetchOrgReposError (pack $ show e) :| []

getFetchedOrganisation :: Github.Auth -> Text -> [Github.Repo] -> IO (Either (NonEmpty SpecificGithubError) FetchedOrganization)
getFetchedOrganisation gheAuth orgName repos = do
  eitherFetchedRepos <- Local.withPool 10 $ \pool ->
                          Local.parallel pool $ fmap (getRepoLabels gheAuth) repos
  let (failures, successes) = partitionEithers eitherFetchedRepos
  pure $ case failures of
    [] -> Right $ FetchedOrganization (FetchedOrgName orgName) successes
    (x:xs) -> Left (x :| xs)

getRepoLabels :: Github.Auth -> Github.Repo -> IO (Either SpecificGithubError FetchedRepo)
getRepoLabels gheAuth repo = do
  let repoOwner = Github.simpleOwnerLogin $ Github.repoOwner repo
      repoName = Github.repoName repo
  labels <- Github.github gheAuth $ Github.labelsOnRepoR repoOwner repoName Github.FetchAll
  pure $ case labels of
    Right xs -> Right $ FetchedRepo (Github.untagName repoName) (VEC.toList xs <&> toFetchedRepoLabel)
    Left e -> Left $ SGFetchRepoLabelError (pack $ show e)

toFetchedRepoLabel :: Github.IssueLabel -> FetchedRepoLabel
toFetchedRepoLabel (Github.IssueLabel labelColour _ labelName) =
  FetchedRepoLabel (Github.untagName labelName) labelColour
