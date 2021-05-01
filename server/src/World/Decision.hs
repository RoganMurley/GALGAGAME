module World.Decision where

import Card (Card(..), Suit(..), cardName)
import Cards (cardsByName)
import Data.Aeson (ToJSON(..), (.=), object)
import Data.Map (Map, fromList)
import Data.String.Conversions (cs)
import Data.Text (Text, intercalate)
import World.WorldProgress (WorldProgress(..), initialProgress)

import qualified Cards
import qualified Data.Map as Map


data Decision = Decision
  { decision_id      :: Text
  , decision_title   :: Text
  , decision_text    :: Text
  , decision_cards   :: [Card]
  , decision_choices :: [DecisionChoice]
  } deriving (Show)


instance ToJSON Decision where
  toJSON (Decision{ decision_id, decision_title, decision_text, decision_cards, decision_choices }) =
    object [
      "id"      .= toJSON decision_id
    , "title"   .= toJSON decision_title
    , "text"    .= toJSON decision_text
    , "cards"   .= toJSON decision_cards
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


removeSuitDecision :: Text -> Text -> Suit -> Decision
removeSuitDecision decisionId text suit =
  Decision
    { decision_id      = decisionId
    , decision_title   = "RENOUNCE"
    , decision_text    = text
    , decision_cards   = []
    , decision_choices =
      [ DecisionChoice "RENOUNCE" dealEff
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
          filter (\name -> nameToSuit name /= Just suit) (worldprogress_deck worldprogress)
      }


renounceSwordDecision :: Decision
renounceSwordDecision = removeSuitDecision "renounceSword" "Renounce the SWORD?" Sword


renounceGrailDecision :: Decision
renounceGrailDecision = removeSuitDecision "renounceGrail" "Renounce the GRAIL?" Grail


renounceWandDecision :: Decision
renounceWandDecision = removeSuitDecision "renounceWand" "Renounce the WAND?" Wand

renounceCoinDecision :: Decision
renounceCoinDecision = removeSuitDecision "renounceCoin" "Renounce the COIN?" Coin


rewardDecision :: Text -> [Card] -> Decision
rewardDecision decisionId cards =
  Decision
    { decision_id      = decisionId
    , decision_title   = "REWARD"
    , decision_text    = intercalate "\n" cardNames
    , decision_cards   = cards
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


defeatDecision :: Decision
defeatDecision =
  Decision
    { decision_id      = "defeat"
    , decision_title   = "INCOMPLETE"
    , decision_text    = "Your journey ends here,\nbut another is just beginning..."
    , decision_cards   = []
    , decision_choices = [
      DecisionChoice "ANOTHER" (\progress -> initialProgress (worldprogress_gen progress))
    ]
    }


resetDecision :: Decision
resetDecision =
  Decision
    { decision_id      = "reset"
    , decision_title   = "Connection Lost"
    , decision_text    = "Your progress was reset."
    , decision_cards   = []
    , decision_choices = [
      DecisionChoice "ANOTHER" (\progress -> progress { worldprogress_decisionId = Nothing })
    ]
    }


completeDecision :: Decision
completeDecision =
  Decision
    { decision_id      = "complete"
    , decision_title   = "COMPLETE"
    , decision_text    = "Your journey was long,\nbut we are finally\nwhole again."
    , decision_cards   = []
    , decision_choices = [
      DecisionChoice "ANOTHER" (\progress -> initialProgress (worldprogress_gen progress))
    ]
    }


tideDecision :: Decision
tideDecision = rewardDecision "tide" [Cards.tideSword, Cards.tideWand, Cards.tideGrail, Cards.tideCoin]


mirrorDecision :: Decision
mirrorDecision = rewardDecision "mirror" [Cards.mirrorSword, Cards.mirrorWand, Cards.mirrorGrail, Cards.mirrorCoin]


mirageDecision :: Decision
mirageDecision = rewardDecision "mirage" [Cards.mirageSword, Cards.mirageWand, Cards.mirageGrail, Cards.mirageCoin]


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
allDecisions =
  [ completeDecision
  , defeatDecision
  , resetDecision
  , tideDecision
  , mirrorDecision
  , blazeDecision
  , heavenDecision
  , shroomDecision
  , dualityDecision
  , alchemyDecision
  , renounceCoinDecision
  , renounceWandDecision
  , renounceGrailDecision
  , renounceSwordDecision
  ]


decisionByIdMap :: Map Text Decision
decisionByIdMap = fromList $ fmap (\decision -> (decision_id decision, decision)) allDecisions


decisionFromId :: Text -> Maybe Decision
decisionFromId decisionId = Map.lookup decisionId decisionByIdMap
