{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Replay.Schema where

import Data.Text (Text)
import Database.Beam (Beamable, Columnar, Generic, Identity, PrimaryKey(..), Table)


data ReplayT f = Replay
  { replayId     :: Columnar f Int
  , replayReplay :: Columnar f Text
  } deriving (Generic)

type Replay = ReplayT Identity
type ReplayId = PrimaryKey ReplayT Identity

deriving instance Show Replay
deriving instance Eq Replay

instance Beamable ReplayT
instance Beamable (PrimaryKey ReplayT)

instance Table ReplayT where
  data PrimaryKey ReplayT f = ReplayId (Columnar f Int) deriving Generic
  primaryKey = ReplayId . replayId
