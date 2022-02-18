module Feedback.Apps where

import qualified Auth.Schema
import Config (App, runBeam)
import Data.ByteString (ByteString)
import Data.String.Conversions (cs)
import Database.Beam (default_, insert, insertExpressions, runInsert, val_)
import Feedback.Schema as Schema
import Schema (GalgagameDb (..), galgagameDb)

saveFeedback :: Maybe ByteString -> ByteString -> App ()
saveFeedback mUsername body =
  runBeam $
    runInsert $
      insert (feedback galgagameDb) $
        insertExpressions
          [ Schema.Feedback
              default_
              (val_ $ Auth.Schema.UserId $ cs <$> mUsername)
              (val_ $ cs body)
          ]
