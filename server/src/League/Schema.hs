{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module League.Schema where

import Auth.Schema (UserT)
import Data.Int (Int64)
import Database.Beam (Beamable, Columnar, Generic, Identity, Nullable, PrimaryKey, Table (..))

data LeagueT f = League
  { leagueId :: Columnar f Int64,
    leagueUser :: PrimaryKey UserT (Nullable f)
  }
  deriving (Generic)

type League = LeagueT Identity

type LeagueId = PrimaryKey LeagueT Identity

instance Beamable LeagueT

instance Beamable (PrimaryKey LeagueT)

instance Table LeagueT where
  data PrimaryKey LeagueT f = LeagueId (Columnar f Int64) deriving (Generic)
  primaryKey = LeagueId . leagueId
