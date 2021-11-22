module Possibility where

import Player (WhichPlayer)
import qualified DSL.Alpha as Alpha

chooseTimeline :: WhichPlayer -> Alpha.Program ()
