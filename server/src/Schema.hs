{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Schema where

import Database.Beam (Database, DatabaseSettings, Generic, TableEntity, defaultDbSettings)

import Auth.Schema (UserT)
import Feedback.Schema (FeedbackT)
import Replay.Schema (ReplayT)
import Stats.Schema (StatsT)
import World.Schema (ProgressT)

data GalgagameDb f = GalgagameDb
  { replays  :: f (TableEntity ReplayT)
  , users    :: f (TableEntity UserT)
  , stats    :: f (TableEntity StatsT)
  , feedback :: f (TableEntity FeedbackT)
  , progress :: f (TableEntity ProgressT)
  } deriving (Generic)

instance Database be GalgagameDb

galgagameDb :: DatabaseSettings be GalgagameDb
galgagameDb = defaultDbSettings
