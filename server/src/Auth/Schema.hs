{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Auth.Schema where

import Data.Int (Int64)
import Data.Text (Text)
import Database.Beam (Beamable, Columnar, Generic, Identity, PrimaryKey, Table (..))

data UserT f = User
  { userId :: Columnar f Int64,
    userEmail :: Columnar f Text,
    userUsername :: Columnar f Text,
    userPasshash :: Columnar f Text,
    userContactable :: Columnar f Bool,
    userSuperuser :: Columnar f Bool
  }
  deriving (Generic)

type User = UserT Identity

type UserId = PrimaryKey UserT Identity

deriving instance Show User

deriving instance Eq User

deriving instance Show UserId

deriving instance Eq UserId

instance Beamable UserT

instance Beamable (PrimaryKey UserT)

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Int64) deriving (Generic)
  primaryKey = UserId . userId
