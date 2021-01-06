{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module World.Schema where

import Data.Text (Text)
import Database.Beam (Beamable, Columnar, Generic, Identity, PrimaryKey(..), Table)

import Auth.Schema (UserT)


data ProgressT f = Progress
  { progressUser  :: PrimaryKey UserT f
  , progressState :: Columnar f Text
  } deriving (Generic)

type Progress = ProgressT Identity
type ProgressId = PrimaryKey ProgressT Identity

instance Beamable ProgressT
instance Beamable (PrimaryKey ProgressT)

instance Table ProgressT where
  data PrimaryKey ProgressT f = ProgressId (PrimaryKey UserT f) deriving Generic
  primaryKey = ProgressId . progressUser
