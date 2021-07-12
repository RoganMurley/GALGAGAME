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
import Util (Gen, random)

import qualified Data.GUID as GUID


data Encounter = Encounter
  { encounter_guid      :: Text
  , encounter_x         :: Float
  , encounter_y         :: Float
  , encounter_key       :: WorldKey
  , encounter_variant   :: Variant
  } deriving (Eq, Show)


data Variant = PvpVariant | CpuVariant CpuVariant
  deriving (Eq, Show)


data CpuVariant = CpuVariantConstructor
  { cpuVariant_name      :: Text
  , cpuVariant_cardNames :: Maybe [Text]
  , cpuVariant_life      :: Life
  , cpuVariant_decision  :: Maybe Decision
  } deriving (Eq, Show)


instance ToJSON Encounter where
  toJSON (Encounter{ encounter_guid, encounter_x, encounter_y, encounter_variant }) =
    object [
      "guid"    .= encounter_guid
    , "x"       .= encounter_x
    , "y"       .= encounter_y
    , "variant" .= encounter_variant
    ]

instance ToJSON Variant where
  toJSON PvpVariant     = "pvp"
  toJSON (CpuVariant _) = "cpu"



newEncounter :: WorldProgress -> Edge -> Gen -> App Encounter
newEncounter progress (Edge{ edge_tarot, edge_key }) gen = do
  guid <- liftIO GUID.genText
  let (x, y) = getPosition edge_key
  let tarot = edge_tarot progress
  variant <- getTarotVariant tarot gen
  return $ Encounter
    { encounter_guid    = guid
    , encounter_x       = x
    , encounter_y       = y
    , encounter_key     = edge_key
    , encounter_variant = variant
    }


getTarotVariant :: Tarot -> Gen -> App Variant
getTarotVariant tarot gen = do
  let Tarot{ tarot_name, tarot_life, tarot_decision, tarot_pvpChance } = tarot
  let tarotCards = getTarotCards tarot gen
  let cpuVariant = (CpuVariant $
        CpuVariantConstructor
          { cpuVariant_name      = tarot_name
          , cpuVariant_cardNames = Just $ cardName <$> tarotCards
          , cpuVariant_life      = tarot_life
          , cpuVariant_decision  = tarot_decision
          }
        )
  let pvpVariant = PvpVariant
  let variant =
        if random gen < tarot_pvpChance then
          pvpVariant
        else
          cpuVariant
  return variant


encounterName :: Encounter -> Text
encounterName (Encounter{ encounter_variant }) =
  case encounter_variant of
    (CpuVariant (CpuVariantConstructor { cpuVariant_name })) ->
      cpuVariant_name
    _ ->
      "???"


postEncounterDecision :: Encounter -> Maybe Decision
postEncounterDecision (Encounter{ encounter_variant }) =
  case encounter_variant of
    (CpuVariant (CpuVariantConstructor { cpuVariant_decision })) ->
      cpuVariant_decision
    _ ->
      Nothing
