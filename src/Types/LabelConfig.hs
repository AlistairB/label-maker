{-# LANGUAGE DeriveGeneric #-}

module Types.LabelConfig
  ( RawLabelConfig(..)
  , LabelMakerConfig(..)
  , Organization(..)
  , OrganizationRepos(..)
  , OrganizationRepo(..)
  , LabelGroups(..)
  , LabelName(..)
  , LabelColour(..)
  , SyncLabel(..)
  , DeleteLabel(..)
  , RenameLabel(..)
  ) where

import GHC.Generics (Generic)
import Data.Yaml
import Data.Aeson.Types

import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (maybeToList)
import Data.HashMap.Strict (toList)

newtype RawLabelConfig = RawLabelConfig { unRawLabelConfig :: Text }
  deriving (Eq, Show)

data LabelMakerConfig = LabelMakerConfig
  { _organizations :: NonEmpty Organization
  , _labelGroups :: LabelGroups
  }
  deriving (Eq, Show)

data Organization = Organization
  { _orgName :: Text
  , _orgRepos :: OrganizationRepos
  }
  deriving (Eq, Show, Generic)

data OrganizationRepos =
    OrganizationReposAll
  | OrganizationReposSpecific (NonEmpty OrganizationRepo)
  deriving (Eq, Show)

newtype OrganizationRepo = OrganizationRepo { unOrganizationRepo :: Text } deriving (Eq, Show, Generic)

data LabelGroups = LabelGroups
  { _sync :: [SyncLabel]
  , _delete :: [DeleteLabel]
  , _rename :: [RenameLabel]
  }
  deriving (Eq, Show)

newtype LabelName = LabelName { unLabelName :: Text } deriving (Eq, Show, Generic)

newtype LabelColour = LabelColour { unLabelColour :: Text } deriving (Eq, Show, Generic)

data SyncLabel = SyncLabel
  { _syncLabelName :: LabelName
  , _syncLabelColor :: LabelColour
  }
  deriving (Eq, Show)

newtype DeleteLabel = DeleteLabel { unDeleteLabel :: LabelName } deriving (Generic, Eq, Show)

data RenameLabel = RenameLabel
  { _renameLabelFrom :: LabelName
  , _renameLabelTo :: LabelName
  }
  deriving (Eq, Show)

instance FromJSON LabelColour where
  parseJSON = withObject "LabelColour" $
    \v -> LabelColour <$> v .: "color"

instance FromJSON OrganizationRepos where
  parseJSON (String "all") = pure OrganizationReposAll
  parseJSON v@(Array _) = OrganizationReposSpecific <$> parseJSON v
  parseJSON invalid =
    prependFailure ("parsing OrganizationRepos failed, " <> show invalid)
        (typeMismatch "Array or \"all\"" invalid)

instance FromJSON OrganizationRepo where
  parseJSON (String repo) = pure $ OrganizationRepo repo
  parseJSON invalid =
    prependFailure ("parsing organization repo failed, " <> show invalid)
        (typeMismatch "String" invalid)

instance FromJSON Organization where
  parseJSON (String orgName) = pure $ Organization orgName OrganizationReposAll
  parseJSON (Object v) = do
        let (name, body) = head . toList $ v
        repos <- withObject "Repos" (.: "repos") body
        pure $ Organization name repos
  parseJSON invalid =
    prependFailure ("parsing organization failed, " <> show invalid)
        (typeMismatch "String" invalid)

instance FromJSON LabelMakerConfig where
  parseJSON = withObject "LabelMakerConfig" $ \v -> do
    orgs <- v .: "organizations"
    labels <- v .: "labels"
    orgsResult <- parseJSON orgs
    pure $ LabelMakerConfig orgsResult labels

instance FromJSON LabelGroups where
  parseJSON = withObject "LabelGroups" $ \v -> do
    sync' <- v .: "sync"
    sync <- traverse pairToSyncLabel (toList sync')
    delete <- fmap (concat . maybeToList) $ v .:? "delete"
    rename <- fmap (concat . maybeToList) $ v .:? "rename"
    pure $ LabelGroups sync delete rename
      where
        pairToSyncLabel :: (Text, Value) -> Parser SyncLabel
        pairToSyncLabel (k, v) = withText "LabelColour" (pure . SyncLabel (LabelName k) . LabelColour) v

instance FromJSON LabelName where
  parseJSON (String label) = pure $ LabelName label
  parseJSON invalid =
    prependFailure ("parsing organisation repo failed, " <> show invalid)
        (typeMismatch "String" invalid)

instance FromJSON DeleteLabel where
  parseJSON (String label) = pure $ DeleteLabel (LabelName label)
  parseJSON invalid =
    prependFailure ("parsing delete label failed, " <> show invalid)
        (typeMismatch "String" invalid)

instance FromJSON RenameLabel where
  parseJSON = withObject "RenameLabel" $ \o ->
    RenameLabel <$> o .: "old-label-name" <*> o .: "new-label-name"
