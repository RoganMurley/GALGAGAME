module World.Tarot where

import Card (Card(..), elementalAspects, mainAspects)
import Cards (cardsByName, getAspectCards)
import Data.Text (Text)
import Life (Life)
import World.Decision (Decision)
import World.WorldProgress (WorldProgress(..), getInitialPairing)
import Util (Gen, randomChoice, shuffle)

import qualified Card
import World.Decision as Decision


data Tarot =
  Tarot
  { tarot_name     :: Text
  , tarot_life     :: Life
  , tarot_cards    :: Maybe [Card]
  , tarot_decision :: Maybe Decision
  } deriving (Eq, Show)


getTarotCards :: Tarot -> Gen -> [Card]
getTarotCards (Tarot{ tarot_cards }) gen =
  case tarot_cards of
    Just cards ->
      cards
    Nothing ->
      let
        cards = concat $ getAspectCards <$> (take 3 $ shuffle gen mainAspects)
      in
        cards >>= replicate 3


tarotBeginning :: WorldProgress -> Tarot
tarotBeginning (WorldProgress{ worldprogress_gen }) =
  let
    (_, aspect) = getInitialPairing worldprogress_gen
    decision =
      case aspect of
        Card.Heaven ->
          Just heavenDecision
        Card.Tide ->
          Just tideDecision
        Card.Shroom ->
          Just shroomDecision
        Card.Blaze ->
          Just blazeDecision
        _ ->
          Nothing
    cards = getAspectCards aspect >>= replicate 3
  in
    Tarot
      "Beginning"
      20
      (Just cards)
      decision


tarotFool :: WorldProgress -> Tarot
tarotFool (WorldProgress{ worldprogress_gen }) =
  let
    cards =
      (getAspectCards Card.Mirror >>= replicate 2) ++
      getAspectCards (randomChoice worldprogress_gen elementalAspects)
  in
    Tarot
      "Fool"
      30
      (Just cards)
      (Just mirrorDecision)


tarotMagician :: WorldProgress -> Tarot
tarotMagician (WorldProgress{ worldprogress_gen }) =
  let
    cards =
      (getAspectCards Card.Alchemy >>= replicate 3) ++
      getAspectCards (randomChoice worldprogress_gen elementalAspects)
  in
    Tarot
      "Magician"
      30
      (Just cards)
      (Just alchemyDecision)


tarotPriestess :: WorldProgress -> Tarot
tarotPriestess (WorldProgress{ worldprogress_gen }) =
  let
    cards =
      (getAspectCards Card.Mirage >>= replicate 3) ++
      getAspectCards (randomChoice worldprogress_gen elementalAspects)
  in
    Tarot
      "Priestess"
      30
      (Just cards)
      (Just mirageDecision)


tarotEmpress :: WorldProgress -> Tarot
tarotEmpress _ =
  Tarot
    "Empress"
    50 Nothing
    Nothing


tarotEmperor :: WorldProgress -> Tarot
tarotEmperor _ =
  Tarot
    "Emperor"
    50
    Nothing
    Nothing


tarotHierophant :: WorldProgress -> Tarot
tarotHierophant (WorldProgress{ worldprogress_gen }) =
  let
    cards =
      (getAspectCards Card.Morph >>= replicate 4) ++
      getAspectCards (randomChoice worldprogress_gen elementalAspects)
  in
    Tarot
      "Hierophant"
      50
      (Just cards)
      Nothing


tarotLovers :: WorldProgress -> Tarot
tarotLovers (WorldProgress{ worldprogress_gen }) =
  let
    cards =
      (getAspectCards Card.Duality >>= replicate 4) ++
      getAspectCards (randomChoice worldprogress_gen elementalAspects)
  in
    Tarot
      "Lovers"
      50
      (Just cards)
      (Just dualityDecision)


tarotChariot :: WorldProgress -> Tarot
tarotChariot _ =
  Tarot
    "Chariot"
    50
    Nothing
    Nothing


tarotJustice :: WorldProgress -> Tarot
tarotJustice _ =
  Tarot
    "Justice"
    50
    Nothing
    Nothing


tarotHermit :: WorldProgress -> Tarot
tarotHermit _ =
  Tarot
    "Hermit"
    50
    Nothing
    (Just Decision.renounceCoinDecision)


tarotWheel :: WorldProgress -> Tarot
tarotWheel _ =
  Tarot
    "Wheel of Fortune"
    50
    Nothing
    Nothing


tarotStrength :: WorldProgress -> Tarot
tarotStrength _ =
  Tarot
    "Strength"
    50
    Nothing
    Nothing


tarotHanged :: WorldProgress -> Tarot
tarotHanged _ =
  Tarot
    "Hanged Man" 50
    Nothing
    Nothing


tarotDeath :: WorldProgress -> Tarot
tarotDeath _ =
  Tarot
    "Death"
    50
    Nothing
    (Just Decision.renounceSwordDecision)


tarotTemperance :: WorldProgress -> Tarot
tarotTemperance _ =
  Tarot
    "Temperance"
    50
    Nothing
    (Just Decision.renounceGrailDecision)


tarotDevil :: WorldProgress -> Tarot
tarotDevil _ =
  Tarot
    "Devil"
    50
    Nothing
    (Just Decision.renounceWandDecision)


tarotTower :: WorldProgress -> Tarot
tarotTower _ =
  Tarot
    "Tower"
    50
    Nothing
    Nothing


tarotStar :: WorldProgress -> Tarot
tarotStar _ = Tarot "Star" 50 Nothing Nothing


tarotMoon :: WorldProgress -> Tarot
tarotMoon _ = Tarot "Moon" 50 Nothing Nothing


tarotSun :: WorldProgress -> Tarot
tarotSun _ = Tarot "Sun" 50 Nothing Nothing


tarotJudgement :: WorldProgress -> Tarot
tarotJudgement _ = Tarot "Judgement" 50 Nothing Nothing


tarotWorld :: WorldProgress -> Tarot
tarotWorld _ = Tarot "World" 50 Nothing Nothing
