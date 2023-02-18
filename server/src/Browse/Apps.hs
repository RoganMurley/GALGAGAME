module Browse.Apps where

import Config (App)
import Data.Text (Text)

data Game = Game
  { game_name :: Text
  }
  deriving (Eq, Show)

loadGames :: App [Game]
loadGames = return []
