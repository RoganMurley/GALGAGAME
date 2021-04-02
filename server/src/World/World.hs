{-# LANGUAGE DeriveGeneric #-}
module World.World where

import Card (Card(..), Suit(..), cardName)
import Cards (cardsByName)
import Config (App, runBeam)
import Control.Concurrent.STM.TVar (TVar)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.=), eitherDecode, encode, object, withObject)
import Data.List (find)
import Data.Map (Map, fromList)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.String.Conversions (cs)
import Data.Text (Text, intercalate)
import Database.Beam ((==.), all_, filter_, insert, insertValues, runInsert, runSelectReturningOne, runUpdate, save, select, val_)
import GHC.Generics
import Life (initMaxLife)
import Player (WhichPlayer(..))
import Safe (readMay)
import Scenario (Scenario(..))
import Schema (GalgagameDb(..), galgagameDb)
import Start (startProgram)
import Text.Printf (printf)
import Util (breakAt)

import qualified Auth.Schema
import qualified Cards
import qualified Data.GUID as GUID
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified DeckBuilding
import qualified Log
import qualified Server
import qualified World.Schema as Schema

type Pos = (Float, Float)


data World = World
  { world_encounters    :: [Encounter]
  , world_others        :: [Pos]
  , world_edgePositions :: [(Pos, Pos)]
  , world_visited       :: [Pos]
  , world_visitedEdges  :: [(Pos, Pos)]
  , world_lockedEdges   :: [(Pos, Pos)]
  , world_decision      :: Maybe Decision
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


getWorld :: TVar Server.State -> WorldProgress -> App World
getWorld _ progress = do
  let key = worldprogress_key progress
  let adjEdges = getAdjEdges key
  let visitedKeys = worldprogress_visited progress
  let edges = filter (\Edge{ edge_key } -> not $ Set.member edge_key visitedKeys) adjEdges
  encounters <- mapM newEncounter edges
  let edgeKeys = Set.fromList $ edge_key <$> edges :: Set WorldKey
  let otherKeys = Set.difference mainKeys (Set.union edgeKeys visitedKeys) :: Set WorldKey
  let others = getPosition <$> Set.toList otherKeys :: [Pos]
  return $
    World
    { world_encounters    = encounters
    , world_others        = others
    , world_edgePositions = getEdgePositions key edgeKeys
    , world_visited       = getPosition <$> Set.toList visitedKeys
    , world_visitedEdges  = worldprogress_visitedEdges progress
    , world_lockedEdges   = lockedEdgePositions
    , world_decision      = worldprogress_decisionId progress >>= decisionFromId
    }


instance ToJSON World where
  toJSON (World{ world_decision, world_encounters, world_others, world_edgePositions, world_lockedEdges, world_visited, world_visitedEdges }) =
    object [
      "encounters"   .= toJSON world_encounters
    , "others"       .= toJSON world_others
    , "edges"        .= toJSON world_edgePositions
    , "visited"      .= toJSON world_visited
    , "visitedEdges" .= toJSON world_visitedEdges
    , "lockedEdges"  .= toJSON world_lockedEdges
    , "decision"     .= toJSON world_decision
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


data WorldRequest
  = JoinEncounter Text
  | EncounterDecision Int
  deriving (Eq, Show)


parseRequest :: Text -> Maybe WorldRequest
parseRequest msg =
  let
    (command, content) = breakAt ":" msg :: (Text, Text)
  in
    case command of
      "joinEncounter" ->
        Just . JoinEncounter $ content
      "encounterDecision" ->
        case readMay $ cs content of
          Just choice ->
            Just . EncounterDecision $ choice
          Nothing ->
            Nothing
      _ ->
        Nothing


makeScenario :: WorldProgress -> Encounter -> Scenario
makeScenario (WorldProgress{ worldprogress_deck }) (Encounter{ encounter_numeral }) =
  Scenario {
    scenario_turn = PlayerA
  , scenario_characterPa = characterPa
  , scenario_characterPb = characterPb
  , scenario_prog = startProgram PlayerA
  , scenario_xpWin = 100
  , scenario_xpLoss = 70
  , scenario_reward = reward
  }
  where
    characterPa :: Maybe DeckBuilding.Character
    characterPa =
      case encounter_numeral of
        "S" ->
          Just $
            DeckBuilding.Character
              "The Beginning"
              ""
              (Left (DeckBuilding.mirrorRune, DeckBuilding.mirrorRune, DeckBuilding.mirrorRune))
              20
        "0" ->
          Just $
            DeckBuilding.Character
              "The Fool"
              ""
              (Left (DeckBuilding.shroomRune, DeckBuilding.shroomRune, DeckBuilding.shroomRune))
              30
        "I" ->
          Just $
            DeckBuilding.Character
              "The Magician"
              ""
              (Left (DeckBuilding.blazeRune, DeckBuilding.blazeRune, DeckBuilding.blazeRune))
              30
        "II" ->
          Just $
            DeckBuilding.Character
              "The Priestess"
              ""
              (Left (DeckBuilding.heavenRune, DeckBuilding.heavenRune, DeckBuilding.heavenRune))
              30
        "V" ->
          Just $
            DeckBuilding.Character
              "The Hierophant"
              ""
              (Left (DeckBuilding.morphRune, DeckBuilding.morphRune, DeckBuilding.morphRune))
              initMaxLife
        "VI" ->
          Just $
            DeckBuilding.Character
              "The Lovers"
              ""
              (Left (DeckBuilding.dualityRune, DeckBuilding.dualityRune, DeckBuilding.dualityRune))
              initMaxLife
        "X" ->
          Just $
            DeckBuilding.Character
              "Wheel of Fortune"
              ""
              (Left (DeckBuilding.alchemyRune, DeckBuilding.alchemyRune, DeckBuilding.alchemyRune))
              initMaxLife
        _ ->
          Nothing
    characterPb :: Maybe DeckBuilding.Character
    characterPb =
      Just $
        DeckBuilding.Character
          "Prideful Fool"
          ""
          (Right $ catMaybes $ (\name -> Map.lookup name cardsByName) <$> worldprogress_deck)
          initMaxLife
    reward =
      case encounter_numeral of
        -- "S" ->
        --   Just [Cards.mirrorSword, Cards.mirrorWand, Cards.mirrorGrail, Cards.mirrorCoin]
        -- "0" ->
        --   Just [Cards.shroomSword, Cards.shroomWand, Cards.shroomGrail, Cards.shroomCoin]
        -- "I" ->
        --   Just [Cards.heavenSword, Cards.heavenWand, Cards.heavenGrail, Cards.heavenCoin]
        -- "II" ->
        --   Just [Cards.blazeSword, Cards.blazeWand, Cards.blazeGrail, Cards.blazeCoin]
        _ ->
         Nothing


-- Graph nonsense
data WorldTree =
  WorldTree
  { worldtree_start         :: WorldNode
  , worldtree_crown         :: WorldNode
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


data WorldKey
  = Start
  | Crown
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


mainKeys :: Set WorldKey
mainKeys =
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
  { worldtree_start         = start
  , worldtree_crown         = crown
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
  start         = WorldNode [Edge tarotBeginning Crown]
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
getEdges Start         = worldtree_start
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


getAdjEdges :: WorldKey -> [Edge]
getAdjEdges key = worldnode_edges $ getEdges key worldTree


getEdgePositions :: WorldKey -> Set WorldKey -> [(Pos, Pos)]
getEdgePositions key edgeKeys = zip (repeat startPosition) endPositions
  where
    startPosition :: Pos
    startPosition = getPosition key
    endPositions :: [Pos]
    endPositions = getPosition <$> (Set.toList $ edgeKeys)


lockedEdgePositions :: [(Pos, Pos)]
lockedEdgePositions = Set.toList $ Set.unions lockedEdges
  where
    lockedEdges :: [Set (Pos, Pos)]
    lockedEdges = getLocked <$> (Set.toList mainKeys)
    getLocked :: WorldKey -> Set (Pos, Pos)
    getLocked key = Set.fromList $ getEdgePositions key (Set.fromList $ edge_key <$> getAdjEdges key)


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


getPosition :: WorldKey -> Pos
getPosition Start         = (0.5, level0)
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


getNewProgress :: Encounter -> Scenario -> WorldProgress -> WorldProgress
getNewProgress encounter scenario progress =
  WorldProgress
    { worldprogress_key          = encounter_key
    , worldprogress_visited      = Set.insert encounter_key worldprogress_visited
    , worldprogress_visitedEdges = edge : worldprogress_visitedEdges
    , worldprogress_deck         = deck
    , worldprogress_decisionId   = decision_id <$> decision
    }
  where
    Encounter{ encounter_key, encounter_x, encounter_y } = encounter
    WorldProgress{ worldprogress_deck, worldprogress_key, worldprogress_visited, worldprogress_visitedEdges } = progress
    Scenario{ scenario_reward } = scenario
    edge :: (Pos, Pos)
    edge = (getPosition worldprogress_key, (encounter_x, encounter_y))
    deck =
      case scenario_reward of
        Just rewardCards ->
          (cardName <$> rewardCards) ++ worldprogress_deck
        Nothing ->
          worldprogress_deck
    decision :: Maybe Decision
    decision = decisionFromEncounter encounter


-- Tarot
data Tarot =
  Tarot
  { tarot_name    :: Text
  , tarot_numeral :: Text
  } deriving (Eq, Show)


tarotBeginning :: Tarot
tarotBeginning = Tarot "The Beginning" "S"


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
  { worldprogress_key          :: WorldKey
  , worldprogress_visited      :: Set WorldKey
  , worldprogress_visitedEdges :: [(Pos, Pos)]
  , worldprogress_deck         :: [Text]
  , worldprogress_decisionId   :: Maybe Text
  } deriving (Show)


instance ToJSON WorldProgress where
  toJSON (WorldProgress{ worldprogress_key, worldprogress_visited, worldprogress_visitedEdges, worldprogress_deck, worldprogress_decisionId }) =
    object [
      "key"          .= worldprogress_key
    , "visited"      .= worldprogress_visited
    , "visitedEdges" .= worldprogress_visitedEdges
    , "deck"         .= worldprogress_deck
    , "decisionId"   .= worldprogress_decisionId
    ]

instance FromJSON WorldProgress where
  parseJSON =
    withObject "WorldProgress" $
    \o ->
      WorldProgress
        <$> o .: "key"
        <*> o .: "visited"
        <*> o .: "visitedEdges"
        <*> o .: "deck"
        <*> o .: "decisionId"


initialProgress :: WorldProgress
initialProgress =
  WorldProgress
    Start
    Set.empty
    []
    (cardName <$> [Cards.basicSword, Cards.basicWand, Cards.basicGrail, Cards.basicCoin])
    Nothing


startProgress :: WorldProgress
startProgress = initialProgress { worldprogress_decisionId = Just "start" }


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
loadProgress Nothing         = return startProgress
loadProgress (Just username) = do
  result <- runBeam $ runSelectReturningOne $
    select $ filter_ (\row -> Schema.progressUser row ==. val_ (Auth.Schema.UserId username)) $
      all_ $ progress galgagameDb
  let progressState = Schema.progressState <$> result
  liftIO $ Log.info $ printf "progressState: %s" (show progressState)
  case progressState of
    Just state -> do
      let decoded = eitherDecode $ cs state :: Either String WorldProgress
      case decoded of
        Left err -> do
          liftIO $ Log.error $ printf "Error loading world progress: %s" err
          return startProgress
        Right progress ->
          return progress
    Nothing ->
      return startProgress


-- Decision
data Decision = Decision
  { decision_id      :: Text
  , decision_title   :: Text
  , decision_text    :: Text
  , decision_choices :: [DecisionChoice]
  } deriving (Show)


instance ToJSON Decision where
  toJSON (Decision{ decision_id, decision_title, decision_text, decision_choices }) =
    object [
      "id"      .= toJSON decision_id
    , "title"   .= toJSON decision_title
    , "text"    .= toJSON decision_text
    , "choices" .= toJSON decision_choices
    ]


instance Eq Decision where
  a == b = decision_id a == decision_id b


data DecisionChoice = DecisionChoice
  { decisionchoice_text :: Text
  , decisionchoice_eff  :: WorldProgress -> WorldProgress
  }


instance ToJSON DecisionChoice where
  toJSON (DecisionChoice{ decisionchoice_text }) =
    object [ "text" .= toJSON decisionchoice_text ]


instance Show DecisionChoice where
  show decisionChoice = cs $ decisionchoice_text decisionChoice


devilDecision :: Decision
devilDecision =
  Decision
    { decision_id      = "devil"
    , decision_title   = "DEVIL"
    , decision_text    = "\"Your grails overflow.\nI'll take them off your hands,\n for a price.\""
    , decision_choices =
      [ DecisionChoice "CLAIM" dealEff
      , DecisionChoice "REJECT" id
      ]
    }
  where
    nameToSuit :: Text -> Maybe Suit
    nameToSuit name = card_suit <$> Map.lookup name cardsByName
    dealEff :: WorldProgress -> WorldProgress
    dealEff worldprogress =
      worldprogress {
        worldprogress_deck =
          filter (\name -> nameToSuit name /= Just Grail) (worldprogress_deck worldprogress)
      }


rewardDecision :: Text -> [Card] -> Decision
rewardDecision decisionId cards =
  Decision
    { decision_id      = decisionId
    , decision_title   = "REWARD"
    , decision_text    = intercalate "\n" cardNames
    , decision_choices =
      [ DecisionChoice "CLAIM" dealEff
      , DecisionChoice "REJECT" id
      ]
    }
  where
    cardNames :: [Text]
    cardNames = cardName <$> cards
    dealEff :: WorldProgress -> WorldProgress
    dealEff worldprogress =
      worldprogress {
        worldprogress_deck = worldprogress_deck worldprogress ++ cardNames
      }


startDecision :: Decision
startDecision =
  Decision
    { decision_id      = "start"
    , decision_title   = "GALGA"
    , decision_text    = "Your journey begins."
    , decision_choices = [
      DecisionChoice "BEGIN" (const initialProgress)
    ]
    }


defeatDecision :: Decision
defeatDecision =
  Decision
    { decision_id      = "defeat"
    , decision_title   = "DEFEAT"
    , decision_text    = "Your journey ends here."
    , decision_choices = [ DecisionChoice "ANOTHER" (const initialProgress) ]
    }


mirrorDecision :: Decision
mirrorDecision = rewardDecision "mirror" [Cards.mirrorSword, Cards.mirrorWand, Cards.mirrorGrail, Cards.mirrorCoin]


blazeDecision :: Decision
blazeDecision = rewardDecision "blaze" [Cards.blazeSword, Cards.blazeWand, Cards.blazeGrail, Cards.blazeCoin]


heavenDecision :: Decision
heavenDecision = rewardDecision "heaven" [Cards.heavenSword, Cards.heavenWand, Cards.heavenGrail, Cards.heavenCoin]


shroomDecision :: Decision
shroomDecision = rewardDecision "shroom" [Cards.shroomSword, Cards.shroomWand, Cards.shroomGrail, Cards.shroomCoin]


dualityDecision :: Decision
dualityDecision = rewardDecision "duality" [Cards.dualitySword, Cards.dualityWand, Cards.dualityGrail, Cards.dualityCoin]


alchemyDecision :: Decision
alchemyDecision = rewardDecision "alchemy" [Cards.alchemySword, Cards.alchemyWand, Cards.alchemyGrail, Cards.alchemyCoin]


allDecisions :: [Decision]
allDecisions = [startDecision, defeatDecision, devilDecision, mirrorDecision, blazeDecision, heavenDecision, shroomDecision, dualityDecision, alchemyDecision]


decisionByIdMap :: Map Text Decision
decisionByIdMap = fromList $ fmap (\decision -> (decision_id decision, decision)) allDecisions


decisionFromId :: Text -> Maybe Decision
decisionFromId decisionId = Map.lookup decisionId decisionByIdMap


decisionFromEncounter :: Encounter -> Maybe Decision
decisionFromEncounter (Encounter{ encounter_numeral }) =
  case encounter_numeral of
    "S" ->
      Just mirrorDecision
    "0" ->
      Just shroomDecision
    "I" ->
      Just blazeDecision
    "II" ->
      Just heavenDecision
    "VI" ->
      Just dualityDecision
    "X" ->
      Just alchemyDecision
    "XV" ->
      Just devilDecision
    _ ->
      Nothing
