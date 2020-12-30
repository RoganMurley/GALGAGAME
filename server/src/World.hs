module World where

import Control.Concurrent.STM.TVar (TVar)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON(..), (.=), object)
import Data.Text (Text)
import Util (breakAt, getGen, shuffle)

import Config (App)

import qualified Data.GUID as GUID
import qualified Server


data World = World [Encounter]
  deriving (Eq, Show)


data Encounter = Encounter
  { encounter_guid :: Text
  , encounter_name :: Text
  , encounter_x    :: Float
  , encounter_y    :: Float
  } deriving (Eq, Show)


encounterNames :: [Text]
encounterNames =
  [ "The Fool"
  , "The Magician"
  , "The Priestess"
  , "The Empress"
  , "The Emperor"
  , "The Hierophant"
  , "The Lovers"
  , "The Chariot"
  , "Justice"
  , "The Hermit"
  , "Wheel of Fortune"
  , "Strength"
  , "The Hanged Man"
  , "Death"
  , "Temperance"
  , "The Devil"
  , "The Tower"
  , "The Star"
  , "The Moon"
  , "The Sun"
  , "Judgement"
  , "The World"
  ]


encounterNumerals :: [Text]
encounterNumerals =
  [ "0"
  , "I"
  , "II"
  , "III"
  , "IV"
  , "V"
  , "VI"
  , "VII"
  , "VIII"
  , "IX"
  , "X"
  , "XI"
  , "XII"
  , "XIII"
  , "XIV"
  , "XV"
  , "XVI"
  , "XVII"
  , "XVIII"
  , "XIX"
  , "XX"
  , "XVI"
  ]


positions :: [(Float, Float)]
positions =
  [ (0.5, 0.2)
  , (0.4, 0.3)
  , (0.6, 0.3)
  , (0.4, 0.45)
  , (0.6, 0.45)
  , (0.4, 0.6)
  , (0.6, 0.6)
  , (0.5, 0.525)
  , (0.5, 0.675)
  , (0.5, 0.825)
  ]


newEncounter :: (Float, Float) -> App Encounter
newEncounter (x, y) = do
  guid <- liftIO GUID.genText
  gen <- liftIO $ getGen
  let name = head $ shuffle gen encounterNumerals
  return $ Encounter
    { encounter_guid = guid
    , encounter_name = name
    , encounter_x    = x
    , encounter_y    = y
    }


getWorld :: TVar Server.State -> App World
getWorld _ = do
  encounters <- mapM newEncounter positions
  return $ World encounters


instance ToJSON World where
  toJSON (World encounters) = toJSON encounters


instance ToJSON Encounter where
  toJSON (Encounter{ encounter_guid, encounter_name, encounter_x, encounter_y }) =
    object [
      "guid" .= encounter_guid
    , "name" .= encounter_name
    , "x"    .= encounter_x
    , "y"    .= encounter_y
    ]


data WorldRequest = JoinEncounter Text
  deriving (Eq, Show)


parseRequest :: Text -> Maybe WorldRequest
parseRequest msg =
  let
    (command, content) = breakAt ":" msg :: (Text, Text)
  in
    case command of
      "joinEncounter" ->
        Just . JoinEncounter $ content
      _ ->
        Nothing
