{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Schema where

import Auth.Schema (UserT)
import Database.Beam (Database, DatabaseSettings, Generic, TableEntity, defaultDbSettings)
import Feedback.Schema (FeedbackT)
import League.Schema (LeagueT)
import Replay.Schema (ReplayT)
import Stats.Schema (StatsT, StatsguestT)

data GalgagameDb f = GalgagameDb
  { replays :: f (TableEntity ReplayT),
    users :: f (TableEntity UserT),
    stats :: f (TableEntity StatsT),
    statsguest :: f (TableEntity StatsguestT),
    feedback :: f (TableEntity FeedbackT),
    league :: f (TableEntity LeagueT)
  }
  deriving (Generic)

instance Database be GalgagameDb

galgagameDb :: DatabaseSettings be GalgagameDb
galgagameDb = defaultDbSettings
