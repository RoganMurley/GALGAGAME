module World.Encounter where

import Card (cardName)
import Config (App)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON(..), (.=), object)
import Data.Text (Text)
import Life (Life)
import World.Decision (Decision)
import World.WorldKey (WorldKey)
import World.WorldProgress (WorldProgress(..))
import World.WorldTree (Edge(..), getPosition)
import World.Tarot (Tarot(..), getTarotCards)

import qualified Data.GUID as GUID


data Encounter = Encounter
  { encounter_guid      :: Text
  , encounter_name      :: Text
  , encounter_cardNames :: Maybe [Text]
  , encounter_life      :: Life
  , encounter_decision  :: Maybe Decision
  , encounter_x         :: Float
  , encounter_y         :: Float
  , encounter_key       :: WorldKey
  } deriving (Eq, Show)


instance ToJSON Encounter where
  toJSON (Encounter{ encounter_guid, encounter_name, encounter_x, encounter_y }) =
    object [
      "guid"    .= encounter_guid
    , "name"    .= encounter_name
    , "x"       .= encounter_x
    , "y"       .= encounter_y
    ]


newEncounter :: WorldProgress -> Edge -> App Encounter
newEncounter progress (Edge{ edge_tarot, edge_key }) = do
  guid <- liftIO GUID.genText
  let (x, y) = getPosition edge_key
  let tarot = edge_tarot progress
  let Tarot{ tarot_name, tarot_life, tarot_decision } = tarot
  let tarotCards = getTarotCards tarot (worldprogress_gen progress)
  return $ Encounter
    { encounter_guid      = guid
    , encounter_name      = tarot_name
    , encounter_cardNames = Just $ cardName <$> tarotCards
    , encounter_life      = tarot_life
    , encounter_decision  = tarot_decision
    , encounter_x         = x
    , encounter_y         = y
    , encounter_key       = edge_key
    }
