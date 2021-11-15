module ArtificialIntelligence where

import GameState (PlayState)
import Player (WhichPlayer)

type Weight = Int

evalState :: WhichPlayer -> PlayState -> Weight
