module Feedback.Apps where

import Auth.Schema qualified
import Config (App, runBeam)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.String.Conversions (cs)
import Database.Beam (default_, insert, insertExpressions, runInsert, val_)
import Feedback.Schema as Schema
import Schema (GalgagameDb (..), galgagameDb)

saveFeedback :: Maybe Int64 -> ByteString -> App ()
saveFeedback mUserId body =
  runBeam $
    runInsert $
      insert (feedback galgagameDb) $
        insertExpressions
          [ Schema.Feedback
              default_
              (val_ $ Auth.Schema.UserId mUserId)
              (val_ $ cs body)
          ]
