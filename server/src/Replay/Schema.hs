{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Replay.Schema where

import Data.Text (Text)
import Database.Beam (Beamable, Columnar, Generic, Identity, Nullable, PrimaryKey(..), Table)

import Auth.Schema (UserT)


data ReplayT f = Replay
  { replayId      :: Columnar f Int
  , replayReplay  :: Columnar f Text
  , replayPlayerA :: PrimaryKey UserT (Nullable f)
  , replayPlayerB :: PrimaryKey UserT (Nullable f)
  } deriving (Generic)

type Replay = ReplayT Identity
type ReplayId = PrimaryKey ReplayT Identity

instance Beamable ReplayT
instance Beamable (PrimaryKey ReplayT)

instance Table ReplayT where
  data PrimaryKey ReplayT f = ReplayId (Columnar f Int) deriving Generic
  primaryKey = ReplayId . replayId
