{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Stats.Schema where

import Auth.Schema (UserId, UserT)
import Data.Int (Int64)
import Data.Text (Text)
import Database.Beam (Beamable, Columnar, Generic, Identity, LensFor(..), PrimaryKey, Table (..))

-- Stats
data StatsT f = Stats
  { statsUser :: PrimaryKey UserT f,
    statsExperience :: Columnar f Int64
  }
  deriving (Generic)

type Stats = StatsT Identity

type StatsId = PrimaryKey StatsT Identity

instance Beamable StatsT

instance Beamable (PrimaryKey StatsT)

instance Table StatsT where
  data PrimaryKey StatsT f = StatsId (PrimaryKey UserT f) deriving (Generic)
  primaryKey = StatsId . statsUser

-- StatsGuest
data StatsguestT f = Statsguest
  { statsguestCid :: Columnar f Text,
    statsguestExperience :: Columnar f Int64
  }
  deriving (Generic)

type Statsguest = StatsguestT Identity

type StatsguestId = PrimaryKey StatsguestT Identity

instance Beamable StatsguestT

instance Beamable (PrimaryKey StatsguestT)

instance Table StatsguestT where
  data PrimaryKey StatsguestT f = StatsguestId (Columnar f Text) deriving (Generic)
  primaryKey = StatsguestId . statsguestCid
