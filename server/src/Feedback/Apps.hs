module Feedback.Apps where

import Data.ByteString (ByteString)
import Database.Beam (default_, insert, insertExpressions, runInsert, val_)
import Data.String.Conversions (cs)

import Config (App, runBeam)
import Schema (RingOfWorldsDb(..), ringOfWorldsDb)

import qualified Auth.Schema
import Feedback.Schema as Schema


saveFeedback :: Maybe ByteString -> ByteString -> App ()
saveFeedback mUsername body =
  runBeam $
    runInsert $
      insert (feedback ringOfWorldsDb) $
        insertExpressions [
          Schema.Feedback
            default_
            (val_ $ Auth.Schema.UserId $ cs <$> mUsername)
            (val_ $ cs body)
        ]
