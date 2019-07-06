{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Feedback.Schema where

import Data.Text (Text)
import Database.Beam (Beamable, Columnar, Generic, Identity, Nullable, PrimaryKey(..), Table)

import Auth.Schema (UserT)


data FeedbackT f = Feedback
  { feedbackId   :: Columnar f Int
  , feedbackUser :: PrimaryKey UserT (Nullable f)
  , feedbackBody :: Columnar f Text
  } deriving (Generic)

type Feedback = FeedbackT Identity
type FeedbackId = PrimaryKey FeedbackT Identity

instance Beamable FeedbackT
instance Beamable (PrimaryKey FeedbackT)

instance Table FeedbackT where
  data PrimaryKey FeedbackT f = FeedbackId (Columnar f Int) deriving Generic
  primaryKey = FeedbackId . feedbackId
