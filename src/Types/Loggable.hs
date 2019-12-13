{-# LANGUAGE FlexibleInstances #-}
module Types.Loggable
  () where

import Data.Text (Text)

import Types.LabelConfig
-- import Types.App

class Loggable a where
  getLogs :: a -> [Text]

instance (Loggable a, Loggable b) => Loggable (Either a b) where
  getLogs (Right a) = "Successful!" : getLogs a
  getLogs (Left e) = "Failure!" : getLogs e

instance Loggable LabelMakerConfig where
  getLogs _ = [ "..." ]
