module Types.App
 ( AppError (..)
 , SpecificGithubError (..)
 )where

import Data.Text (Text)
import Data.List.NonEmpty

data AppError =
    ParseFailure
  | ReadFileException
  | GithubErrors (NonEmpty SpecificGithubError)
  deriving (Eq, Show)

data SpecificGithubError =
    SGFetchOrgReposError Text
  | SGFetchOrgRepoError Text
  | SGFetchRepoLabelError Text
  | SGLabelWriteError Text
  deriving (Eq, Show)
