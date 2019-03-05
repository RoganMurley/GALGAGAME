{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Schema where

import Database.Beam (Database, DatabaseSettings, Generic, TableEntity, defaultDbSettings)

import Replay.Schema (ReplayT)
import Auth.Schema (UserT)

data RingOfWorldsDb f = RingOfWorldsDb
  { replays :: f (TableEntity ReplayT)
  , users   :: f (TableEntity UserT)
  } deriving (Generic)

instance Database be RingOfWorldsDb

ringOfWorldsDb :: DatabaseSettings be RingOfWorldsDb
ringOfWorldsDb = defaultDbSettings
