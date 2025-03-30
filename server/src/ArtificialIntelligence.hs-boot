module ArtificialIntelligence where

import GameState (PlayState)
import Player (WhichPlayer)

type Weight = Int

data Weightings = Weightings
  { weightings_life :: Weight,
    weightings_hand :: Weight
  }

evalState :: WhichPlayer -> Weightings -> Weightings -> PlayState -> Weight
