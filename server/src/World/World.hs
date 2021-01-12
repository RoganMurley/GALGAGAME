{-# LANGUAGE DeriveGeneric #-}
module World.World where

import Config (App, runBeam)
import Control.Concurrent.STM.TVar (TVar)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.=), eitherDecode, encode, object, withObject)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Database.Beam ((==.), all_, filter_, insert, insertValues, runInsert, runSelectReturningOne, runUpdate, save, select, val_)
import GHC.Generics
import Player (WhichPlayer(..))
import Scenario (Scenario(..))
import Schema (GalgagameDb(..), galgagameDb)
import Start (startProgram)
import Text.Printf (printf)
import Util (breakAt)

import qualified Auth.Schema
import qualified Data.GUID as GUID
import qualified Data.Set as Set
import qualified DeckBuilding
import qualified Log
import qualified Server
import qualified World.Schema as Schema


data World = World
  { world_encounters    :: [Encounter]
  , world_others        :: [(Float, Float)]
  , world_edgePositions :: [((Float, Float), (Float, Float))]
  }
  deriving (Eq, Show)


data Encounter = Encounter
  { encounter_guid    :: Text
  , encounter_name    :: Text
  , encounter_numeral :: Text
  , encounter_x       :: Float
  , encounter_y       :: Float
  , encounter_key     :: WorldKey
  } deriving (Eq, Show)


encounterFromGuid :: World -> Text -> Maybe Encounter
encounterFromGuid (World{ world_encounters }) guid =
  find
    (\encounter -> encounter_guid encounter == guid)
    world_encounters


newEncounter :: Edge -> App Encounter
newEncounter (Edge{ edge_tarot, edge_key }) = do
  guid <- liftIO GUID.genText
  let (x, y) = getPosition edge_key
  let Tarot{ tarot_name, tarot_numeral } = edge_tarot
  return $ Encounter
    { encounter_guid    = guid
    , encounter_name    = tarot_name
    , encounter_numeral = tarot_numeral
    , encounter_x       = x
    , encounter_y       = y
    , encounter_key     = edge_key
    }


getWorld :: Maybe Text -> TVar Server.State -> Maybe WorldProgress -> App World
getWorld mUsername _ mProgress = do
  progress <- fromMaybe (loadProgress mUsername) (return <$> mProgress)
  let key = worldprogress_key progress
  let adjEdges = worldnode_edges $ getEdges key worldTree
  let visitedKeys = worldprogress_visited progress
  let edges = filter (\Edge{ edge_key } -> not $ Set.member edge_key visitedKeys) adjEdges
  encounters <- mapM newEncounter edges
  let edgeKeys = Set.fromList $ edge_key <$> edges :: Set WorldKey
  let otherKeys = Set.difference allKeys edgeKeys :: Set WorldKey
  let others = getPosition <$> Set.toList otherKeys :: [(Float, Float)]
  let startPos = getPosition key
  let edgePositions = zip (repeat startPos) (getPosition <$> Set.toList edgeKeys)
  return $
    World
    { world_encounters    = encounters
    , world_others        = others
    , world_edgePositions = edgePositions
    }


instance ToJSON World where
  toJSON (World{ world_encounters, world_others, world_edgePositions }) =
    object [
      "encounters" .= toJSON world_encounters
    , "others"     .= toJSON world_others
    , "edges"      .= toJSON world_edgePositions
    ]


instance ToJSON Encounter where
  toJSON (Encounter{ encounter_guid, encounter_name, encounter_numeral, encounter_x, encounter_y }) =
    object [
      "guid"    .= encounter_guid
    , "name"    .= encounter_name
    , "numeral" .= encounter_numeral
    , "x"       .= encounter_x
    , "y"       .= encounter_y
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


makeScenario :: Encounter -> Scenario
makeScenario (Encounter{ encounter_numeral }) =
  Scenario {
    scenario_turn = PlayerA
  , scenario_characterPa = characterPa
  , scenario_characterPb = characterPb
  , scenario_prog = startProgram PlayerA
  , scenario_xpWin = 100
  , scenario_xpLoss = 70
  }
  where
    characterPa :: Maybe DeckBuilding.Character
    characterPa =
      case encounter_numeral of
        "0" ->
          Just $
            DeckBuilding.Character
              "The Fool"
              ""
              DeckBuilding.mirrorRune
              DeckBuilding.mirrorRune
              DeckBuilding.mirrorRune
        "I" ->
          Just $
            DeckBuilding.Character
              "The Magician"
              ""
              DeckBuilding.blazeRune
              DeckBuilding.blazeRune
              DeckBuilding.blazeRune
        "II" ->
          Just $
            DeckBuilding.Character
              "The Priestess"
              ""
              DeckBuilding.heavenRune
              DeckBuilding.heavenRune
              DeckBuilding.heavenRune
        "V" ->
          Just $
            DeckBuilding.Character
              "The Hierophant"
              ""
              DeckBuilding.morphRune
              DeckBuilding.morphRune
              DeckBuilding.morphRune
        "VI" ->
          Just $
            DeckBuilding.Character
              "The Lovers"
              ""
              DeckBuilding.dualityRune
              DeckBuilding.dualityRune
              DeckBuilding.dualityRune
        "X" ->
          Just $
            DeckBuilding.Character
              "Wheel of Fortune"
              ""
              DeckBuilding.blazeRune
              DeckBuilding.blazeRune
              DeckBuilding.blazeRune
        _ ->
          Nothing
    characterPb :: Maybe DeckBuilding.Character
    characterPb = Just DeckBuilding.catherine


-- Graph nonsense
data WorldTree =
  WorldTree
  { worldtree_crown         :: WorldNode
  , worldtree_understanding :: WorldNode
  , worldtree_wisdom        :: WorldNode
  , worldtree_severity      :: WorldNode
  , worldtree_mercy         :: WorldNode
  , worldtree_splendor      :: WorldNode
  , worldtree_victory       :: WorldNode
  , worldtree_beauty        :: WorldNode
  , worldtree_foundation    :: WorldNode
  , worldtree_kingdom       :: WorldNode
  }


data WorldKey =
    Crown
  | Understanding
  | Wisdom
  | Severity
  | Mercy
  | Splendor
  | Victory
  | Beauty
  | Foundation
  | Kingdom
  deriving (Eq, Generic, Ord, Show)


instance ToJSON WorldKey

instance FromJSON WorldKey

data WorldNode =
  WorldNode
  { worldnode_edges :: [Edge]
  } deriving (Eq, Show)


data Edge =
  Edge
  { edge_tarot :: Tarot
  , edge_key   :: WorldKey
  } deriving (Eq, Show)


allKeys :: Set WorldKey
allKeys =
  Set.fromList
    [ Crown
    , Understanding
    , Wisdom
    , Severity
    , Mercy
    , Splendor
    , Victory
    , Beauty
    , Foundation
    , Kingdom
    ]


worldTree :: WorldTree
worldTree =
  WorldTree
  { worldtree_crown         = crown
  , worldtree_understanding = understanding
  , worldtree_wisdom        = wisdom
  , worldtree_severity      = severity
  , worldtree_mercy         = mercy
  , worldtree_splendor      = splendor
  , worldtree_victory       = victory
  , worldtree_beauty        = beauty
  , worldtree_foundation    = foundation
  , worldtree_kingdom       = kingdom
  }
  where
  crown         = WorldNode [Edge tarotMagician Understanding, Edge tarotFool Wisdom, Edge tarotPriestess Beauty]
  understanding = WorldNode [Edge tarotMagician Crown, Edge tarotFool Wisdom, Edge tarotChariot Severity, Edge tarotLovers Beauty]
  wisdom        = WorldNode [Edge tarotFool Crown, Edge tarotEmpress Understanding, Edge tarotHierophant Mercy, Edge tarotEmperor Beauty]
  severity      = WorldNode [Edge tarotChariot Understanding, Edge tarotJustice Mercy, Edge tarotStrength Beauty, Edge tarotHanged Splendor]
  mercy         = WorldNode [Edge tarotHierophant Wisdom, Edge tarotJustice Severity, Edge tarotHermit Beauty, Edge tarotDeath Victory]
  splendor      = WorldNode [Edge tarotHanged Severity, Edge tarotDevil Beauty, Edge tarotSun Foundation, Edge tarotJudgement Kingdom]
  victory       = WorldNode [Edge tarotWheel Mercy, Edge tarotDeath Beauty, Edge tarotStar Foundation, Edge tarotMoon Kingdom]
  beauty        = WorldNode [Edge tarotPriestess Crown, Edge tarotLovers Understanding, Edge tarotEmperor Wisdom, Edge tarotStrength Severity, Edge tarotHermit Mercy, Edge tarotDevil Splendor, Edge tarotDeath Victory, Edge tarotTemperance Foundation]
  foundation    = WorldNode [Edge tarotTemperance Beauty, Edge tarotSun Splendor, Edge tarotStar Victory, Edge tarotWorld Kingdom]
  kingdom       = WorldNode [Edge tarotJudgement Splendor, Edge tarotWorld Foundation, Edge tarotMoon Victory]


getEdges :: WorldKey ->  WorldTree -> WorldNode
getEdges Crown         = worldtree_crown
getEdges Understanding = worldtree_understanding
getEdges Wisdom        = worldtree_wisdom
getEdges Severity      = worldtree_severity
getEdges Mercy         = worldtree_mercy
getEdges Splendor      = worldtree_splendor
getEdges Victory       = worldtree_victory
getEdges Beauty        = worldtree_beauty
getEdges Foundation    = worldtree_foundation
getEdges Kingdom       = worldtree_kingdom


gap :: Float
gap = 0.08
level0 :: Float
level0 = 0.33
level1 :: Float
level1 = level0 + gap
level2 :: Float
level2 = level1 + gap
level3 :: Float
level3 = level2 + gap
levelBeauty0 :: Float
levelBeauty0 = level2 + 0.5 * gap
levelBeauty1 :: Float
levelBeauty1 = levelBeauty0 + gap
levelBeauty2 :: Float
levelBeauty2 = levelBeauty1 + gap


getPosition :: WorldKey -> (Float, Float)
getPosition Crown         = (0.5, level0)
getPosition Understanding = (0.4, level1)
getPosition Wisdom        = (0.6, level1)
getPosition Severity      = (0.4, level2)
getPosition Mercy         = (0.6, level2)
getPosition Splendor      = (0.4, level3)
getPosition Victory       = (0.6, level3)
getPosition Beauty        = (0.5, levelBeauty0)
getPosition Foundation    = (0.5, levelBeauty1)
getPosition Kingdom       = (0.5, levelBeauty2)


getNewProgress :: Encounter -> Maybe WorldProgress -> WorldProgress
getNewProgress (Encounter{ encounter_key }) Nothing =
  WorldProgress
    { worldprogress_key = encounter_key
    , worldprogress_visited = Set.singleton encounter_key
    }
getNewProgress (Encounter{ encounter_key }) (Just (WorldProgress{ worldprogress_visited })) =
  WorldProgress
    { worldprogress_key = encounter_key
    , worldprogress_visited = Set.insert encounter_key worldprogress_visited
    }


-- Tarot
data Tarot =
  Tarot
  { tarot_name    :: Text
  , tarot_numeral :: Text
  } deriving (Eq, Show)


tarotFool :: Tarot
tarotFool = Tarot "The Fool" "0"


tarotMagician :: Tarot
tarotMagician = Tarot "The Magician" "I"


tarotPriestess :: Tarot
tarotPriestess = Tarot "The Priestess" "II"


tarotEmpress :: Tarot
tarotEmpress = Tarot "The Empress" "III"


tarotEmperor :: Tarot
tarotEmperor = Tarot "The Emperor" "IV"


tarotHierophant :: Tarot
tarotHierophant = Tarot "The Hierophant" "V"


tarotLovers :: Tarot
tarotLovers = Tarot "The Lovers" "VI"


tarotChariot :: Tarot
tarotChariot = Tarot "The Chariot" "VII"


tarotJustice :: Tarot
tarotJustice = Tarot "Justice" "VIII"


tarotHermit :: Tarot
tarotHermit = Tarot "The Hermit" "IX"


tarotWheel :: Tarot
tarotWheel = Tarot "Wheel of Fortune" "X"


tarotStrength :: Tarot
tarotStrength = Tarot "Strength" "XI"


tarotHanged :: Tarot
tarotHanged = Tarot "The Hanged Man" "XII"


tarotDeath :: Tarot
tarotDeath = Tarot "Death" "XIII"


tarotTemperance :: Tarot
tarotTemperance = Tarot "Temperance" "XIV"


tarotDevil :: Tarot
tarotDevil = Tarot "The Devil" "XV"


tarotTower :: Tarot
tarotTower = Tarot "The Tower" "XVI"


tarotStar :: Tarot
tarotStar = Tarot "The Star" "XVII"


tarotMoon :: Tarot
tarotMoon = Tarot "The Moon" "XVIII"


tarotSun :: Tarot
tarotSun = Tarot "The Sun" "XIX"


tarotJudgement :: Tarot
tarotJudgement = Tarot "Judgement" "XX"


tarotWorld :: Tarot
tarotWorld = Tarot "The World" "XXI"


-- World Progress
data WorldProgress = WorldProgress
  { worldprogress_key     :: WorldKey
  , worldprogress_visited :: Set WorldKey
  } deriving (Show)

instance ToJSON WorldProgress where
  toJSON (WorldProgress{ worldprogress_key, worldprogress_visited }) =
    object [
      "key"     .= worldprogress_key
    , "visited" .= worldprogress_visited
    ]

instance FromJSON WorldProgress where
  parseJSON =
    withObject "WorldProgress" $
    \o ->
      WorldProgress
        <$> o .: "key"
        <*> o .: "visited"


initialProgress :: WorldProgress
initialProgress = WorldProgress Crown Set.empty


updateProgress :: Maybe Text -> WorldProgress -> App ()
updateProgress (Just username) progressState = do
  result <- runBeam $ runSelectReturningOne $
    select $ filter_ (\row -> Schema.progressUser row ==. val_ (Auth.Schema.UserId username)) $
      all_ $ progress galgagameDb
  let prog = Schema.Progress (Auth.Schema.UserId $ cs username) (cs $ encode progressState)
  case result of
    Just _ -> do
      liftIO $ Log.info $ printf "World progress found, updating..."
      runBeam $ runUpdate $ save (progress galgagameDb) prog
    Nothing -> do
      liftIO $ Log.info $ printf "World progress not found, inserting..."
      runBeam $ runInsert $ insert (progress galgagameDb) $ insertValues [ prog ]
updateProgress Nothing _ = return ()


loadProgress :: Maybe Text -> App WorldProgress
loadProgress Nothing         = return initialProgress
loadProgress (Just username) = do
  result <- runBeam $ runSelectReturningOne $
    select $ filter_ (\row -> Schema.progressUser row ==. val_ (Auth.Schema.UserId username)) $
      all_ $ progress galgagameDb
  return $ fromDb $ Schema.progressState <$> result
  where
    fromDb :: Maybe Text -> WorldProgress
    fromDb mText =
      case mText of
        Just text ->
          case eitherDecode $ cs text of
            Left _ ->
              initialProgress
            Right prog ->
              prog
        Nothing ->
          initialProgress
