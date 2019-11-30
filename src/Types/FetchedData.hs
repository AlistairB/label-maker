module Types.FetchedData
  ( FetchedAllData(..)
  , FetchedRepo(..)
  , FetchedRepoLabel(..)
  , FetchedOrgName(..)
  , FetchedOrganization(..)
  ) where



import Data.Text

newtype FetchedAllData = FetchedAllData
  { _fetchedOrgs :: [FetchedOrganization]
  } deriving (Eq, Show) 

data FetchedRepo = FetchedRepo 
  { _fetchedRepoName :: Text
  , _fetchedRepoLabels :: [FetchedRepoLabel]
  } deriving (Eq, Show)

data FetchedRepoLabel = FetchedRepoLabel
  { _fetchedRepoLabelName :: Text
  , _fetchedRepoLabelColour :: Text
  } deriving (Eq, Show)

newtype FetchedOrgName = FetchedOrgName { unfetchedOrgName :: Text } deriving (Eq, Show)

data FetchedOrganization = FetchedOrganization 
  { _fetchedOrgName :: FetchedOrgName
  , _fetchedRepos :: [FetchedRepo]
  } deriving (Eq, Show)
