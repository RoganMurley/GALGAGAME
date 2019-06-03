{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Stats.Schema where

import Data.Int (Int64)
import Database.Beam (Beamable, Columnar, Generic, Identity, PrimaryKey(..), Table)

import Auth.Schema (UserT)


data StatsT f = Stats
  { statsUser       :: PrimaryKey UserT f
  , statsExperience :: Columnar f Int64
  } deriving (Generic)

type Stats = StatsT Identity
type StatsId = PrimaryKey StatsT Identity

instance Beamable StatsT
instance Beamable (PrimaryKey StatsT)

instance Table StatsT where
  data PrimaryKey StatsT f = StatsId (PrimaryKey UserT f) deriving Generic
  primaryKey = StatsId . statsUser
