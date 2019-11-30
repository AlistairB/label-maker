module Types.RepoUpdatePlan 
  ( LabelMakerUpdatePlan(..)
  , LabelUpdate(..)
  , LabelAction(..)
  ) where

import Data.Text (Text)

newtype LabelMakerUpdatePlan = LabelMakerUpdatePlan { unLabelMakerUpdatePlan :: [LabelUpdate] }
  deriving (Show, Eq)

type OrgName = Text
type RepoName = Text
type OldName = Text
type NewColour = Text
type Colour = Text

data LabelUpdate = LabelUpdate
  { _luOrgName :: OrgName
  , _luRepoName :: RepoName
  , _luTargetLabelName :: Text
  , _luLabelAction :: LabelAction
  } deriving (Show, Eq)

data LabelAction =
    DeleteAction
  | CreateAction Colour
  | UpdateAction OldName NewColour
  deriving (Show, Eq)
