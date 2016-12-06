{-# LANGUAGE OverloadedStrings #-}
module GameState where

import Control.Applicative ((<$>))
import Control.Monad (MonadPlus, mplus)
import Data.Monoid ((<>))
import Data.Aeson (ToJSON(..), (.=), object)
import Data.List (findIndex, partition)
import Data.Maybe (fromJust, isJust, maybeToList)
import Data.Text (Text)
import Safe (headMay, tailSafe)
import System.Random (StdGen, split)
import System.Random.Shuffle (shuffle')


data GameState = Waiting StdGen | Playing Model | Victory WhichPlayer StdGen ResolveList | Draw StdGen ResolveList

data Model = Model Turn Stack Hand Hand Deck Deck Life Life HoverCardIndex HoverCardIndex Passes ResolveList StdGen
type Hand = [Card]
type Deck = [Card]
type Stack = [StackCard]
data Card = Card CardName CardDesc CardImgURL CardEff
data StackCard = StackCard WhichPlayer Card
type CardName = Text
type CardDesc = Text
type CardImgURL = Text
type Life = Int
type CardEff = (WhichPlayer -> Model -> Model)
type ResolveList = [Model]
type HoverCardIndex = Maybe Int

data WhichPlayer = PlayerA | PlayerB
  deriving (Eq, Show)
type Turn = WhichPlayer

data Passes = NoPass | OnePass
  deriving (Eq)

instance ToJSON GameState where
  toJSON (Waiting _) =
    object [
      "waiting" .= True
    ]
  toJSON (Playing model) =
    object [
      "playing" .= toJSON model
    ]
  toJSON (Victory which gen res) =
    object [
      "victory" .= which
    , "res" .= res
    ]
  toJSON (Draw gen res) =
    object [
      "draw" .= True
    , "res" .= res
    ]

instance ToJSON Model where
  toJSON (Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB _ res _) =
    object
      [
        "turn" .= turn
      , "stack" .= stack
      , "handPA" .= handPA
      , "handPB" .= (length handPB)
      , "lifePA" .= lifePA
      , "lifePB" .= lifePB
      , "hoverPA" .= hoverPA
      , "hoverPB" .= hoverPB
      , "res" .= res
      ]

instance ToJSON Card where
  toJSON (Card name desc imageURL eff) =
    object
      [
        "name" .= name
      , "desc" .= desc
      , "imageURL" .= imageURL
      ]

instance ToJSON StackCard where
  toJSON (StackCard owner card) =
    object [
      "owner" .= owner
    , "card" .= card
    ]

instance ToJSON WhichPlayer where
  toJSON PlayerA = "pa"
  toJSON PlayerB = "pb"

data GameCommand =
    EndTurn
  | PlayCard CardName
  | HoverCard CardName
  | Rematch

handMaxLength :: Int
handMaxLength = 6

lifeMax :: Life
lifeMax = 50

initModel :: Turn -> StdGen -> Model
initModel turn gen = Model turn [] handPA handPB deckPA deckPB lifeMax lifeMax Nothing Nothing NoPass [] gen
  where
    (genPA, genPB) = split gen :: (StdGen, StdGen)
    initDeckPA = shuffle' initDeck (length initDeck) genPA :: Deck
    (handPA, deckPA) = splitAt 4 initDeckPA :: (Hand, Deck)
    initDeckPB = shuffle' initDeck (length initDeck) genPB :: Deck
    (handPB, deckPB) = splitAt 4 initDeckPB :: (Hand, Deck)

initDeck :: Deck
initDeck =
  -- DAMAGE
     (replicate 3 cardFireball)
  ++ (replicate 3 cardDagger)
  ++ (replicate 3 cardBoomerang)
  ++ (replicate 3 cardPotion)
  ++ (replicate 3 cardVampire)
  ++ (replicate 3 cardSuccubus)
  -- CONTROL
  ++ (replicate 2 cardSiren)
  ++ (replicate 2 cardSickness)
  -- ++ (replicate 3 cardObscurer)
  -- HARD CONTROL
  ++ (replicate 2 cardHubris)
  ++ (replicate 2 cardReflect)
  ++ (replicate 2 cardReversal)
  ++ (replicate 2 cardConfound)
  -- SOFT CONTROL
  ++ (replicate 2 cardSiren)
  ++ (replicate 2 cardSickness)
  -- ++ (replicate 2 cardEcho)
  ++ (replicate 2 cardProphecy)
  -- ++ (replicate 2 cardOffering)
  -- ++ (replicate 2 cardGoatFlute)


-- TEMP STUFF.
reverso :: GameState -> GameState
reverso (Playing model) = Playing (modelReverso model)
reverso (Victory which gen res) = Victory (otherPlayer which) gen (fmap modelReverso res)
reverso (Draw gen res) = Draw gen (fmap modelReverso res)
reverso (Waiting gen) = Waiting gen


modelReverso :: Model -> Model
modelReverso (Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen) =
  (Model (otherTurn turn) (stackRev stack) handPB handPA deckPB deckPA lifePB lifePA hoverPB hoverPA passes (fmap modelReverso res) gen)
  where
    stackRev :: Stack -> Stack
    stackRev stack = fmap (\(StackCard p c) -> StackCard (otherPlayer p) c) stack


-- UPDATE

update :: GameCommand -> WhichPlayer -> GameState -> GameState
update cmd which state =
  case resetState of
    (Playing model) ->
      case cmd of
        EndTurn ->
          endTurn which model
        PlayCard name ->
          Playing (playCard name which model)
        HoverCard name ->
          Playing (hoverCard name which model)
    (Victory winner gen res) ->
      case cmd of
        Rematch ->
          Playing $ initModel winner (fst (split gen))
        x ->
          Victory winner gen res
    (Draw gen res) ->
      case cmd of
        Rematch ->
          Playing $ initModel PlayerA (fst (split gen))
        x ->
          Draw gen res
    x ->
      x
  where
    resetState :: GameState
    resetState = resetRes state

drawCard :: WhichPlayer -> Model -> Model
drawCard which model@(Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen)
  | (length hand >= handMaxLength) = model
  | otherwise =
    case drawnCard of
      Just card ->
        setDeck which drawnDeck $ setHand which (card : hand) model
      Nothing ->
        model
  where
    drawnCard :: Maybe Card
    drawnCard = headMay deck
    drawnDeck :: Deck
    drawnDeck = tailSafe deck
    deck :: Deck
    deck = getDeck which model
    hand :: Hand
    hand = getHand which model

-- Make safer.
endTurn :: WhichPlayer -> Model -> GameState
endTurn which model@(Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen)
  | turn /= which = Playing model
  | handFull = Playing model
  | otherwise =
    case bothPassed of
      True ->
        case resolveAll (Playing model) of
          Playing m ->
            Playing $ drawCards $ resetPasses $ swapTurn m
          s ->
            s
      False -> Playing $ swapTurn model
  where
    bothPassed :: Bool
    bothPassed = passes == OnePass
    handFull ::  Bool
    handFull = (length (getHand which model)) == handMaxLength
    drawCards :: Model -> Model
    drawCards m = (drawCard PlayerA) . (drawCard PlayerB) $ m


-- Apply a function n times.
times :: Int -> (a -> a) -> a -> a
times n f x = (iterate f x) !! n

-- In future, tag cards in hand with a uid and use that.
playCard :: CardName -> WhichPlayer -> Model -> Model
playCard name which model@(Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen)
  | turn /= which = model
  | otherwise =
    case card of
      Just c ->
        resetPasses $ swapTurn $ modStack ((:) c) $ setHand which newHand model
      Nothing ->
        model
  where
    hand :: Hand
    hand = getHand which model
    (matches, misses) = partition (\(Card n _ _ _) -> n == name) hand :: ([Card], [Card])
    newHand :: Hand
    newHand = (tailSafe matches) ++ misses
    card :: Maybe StackCard
    card = (StackCard which) <$> (headMay matches)

hoverCard :: CardName -> WhichPlayer -> Model -> Model
hoverCard name which model = setHover which cardIndex model
  where
    cardIndex :: Maybe Int
    cardIndex = findIndex (\(Card n _ _ _) -> n == name) (getHand which model)

swapTurn :: Model -> Model
swapTurn model@(Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen) =
  Model (otherTurn turn) stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB (incPasses passes) res gen

incPasses :: Passes -> Passes
incPasses NoPass = OnePass
incPasses OnePass = NoPass

resetPasses :: Model -> Model
resetPasses (Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen) =
  Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB NoPass res gen

otherTurn :: Turn -> Turn
otherTurn PlayerA = PlayerB
otherTurn PlayerB = PlayerA

otherPlayer :: WhichPlayer -> WhichPlayer
otherPlayer = otherTurn

-- RNG GEN.
getGen :: Model -> StdGen
getGen (Model _ _ _ _ _ _ _ _ _ _ _ _ gen) = gen

-- LIFE.
getLife :: WhichPlayer -> Model -> Life
getLife PlayerA (Model _ _ _ _ _ _ lifePA _ _ _ _ _ _) = lifePA
getLife PlayerB (Model _ _ _ _ _ _ _ lifePB _ _ _ _ _) = lifePB

setLife :: WhichPlayer -> Life -> Model -> Model
setLife PlayerA newLife (Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen) =
  Model turn stack handPA handPB deckPA deckPB newLife lifePB hoverPA hoverPB passes res gen
setLife PlayerB newLife (Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen) =
  Model turn stack handPA handPB deckPA deckPB lifePA newLife hoverPA hoverPB passes res gen

modLife :: (Life -> Life) -> WhichPlayer -> Model -> Model
modLife f p m = setLife p (f (getLife p m)) m

-- HAND.
getHand :: WhichPlayer -> Model -> Hand
getHand PlayerA (Model _ _ handPA handPB _ _ _ _ _ _ _ _ _) = handPA
getHand PlayerB (Model _ _ handPA handPB _ _ _ _ _ _ _ _ _) = handPB

setHand :: WhichPlayer -> Hand -> Model -> Model
setHand PlayerA newHand (Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen) =
  Model turn stack newHand handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen
setHand PlayerB newHand (Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen) =
  Model turn stack handPA newHand deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen

mapHand :: (Hand -> a) -> WhichPlayer -> Model -> a
mapHand f p m = f (getHand p m)

modHand :: (Hand -> Hand) -> WhichPlayer -> Model -> Model
modHand f p m = setHand p (f (getHand p m)) m

addToHand :: Card -> Hand -> Hand
addToHand card hand
  | length hand < handMaxLength =
    card : hand
  | otherwise = hand

-- DECK.
getDeck :: WhichPlayer -> Model -> Deck
getDeck PlayerA (Model _ _ _ _ deckPA deckPB _ _ _ _ _ _ _) = deckPA
getDeck PlayerB (Model _ _ _ _ deckPA deckPB _ _ _ _ _ _ _) = deckPB

setDeck :: WhichPlayer -> Deck -> Model -> Model
setDeck PlayerA newDeck (Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen) =
  Model turn stack handPA handPB newDeck deckPB lifePA lifePB hoverPA hoverPB passes res gen
setDeck PlayerB newDeck (Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen) =
  Model turn stack handPA handPB deckPA newDeck lifePA lifePB hoverPA hoverPB passes res gen

mapDeck :: (Deck -> a) -> WhichPlayer -> Model -> a
mapDeck f p m = f (getDeck p m)

modDeck :: (Deck -> Deck) -> WhichPlayer -> Model -> Model
modDeck f p m = setDeck p (mapDeck f p m) m

modDeckHead :: (Card -> Card) -> WhichPlayer -> Model -> Model
modDeckHead f p m =
  case headMay (getDeck p m) of
    Just card ->
      setDeck p ((f card) : (tail (getDeck p m))) m
    Nothing ->
      m

-- STACK.
getStack :: Model -> Stack
getStack (Model _ stack _ _ _ _ _ _ _ _ _ _ _) = stack

setStack :: Stack -> Model -> Model
setStack newStack (Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen) =
  Model turn newStack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen

mapStack :: (Stack -> a) -> Model -> a
mapStack f m = f (getStack m)

modStack :: (Stack -> Stack) -> Model -> Model
modStack f m = setStack (mapStack f m) m

modStackHead :: (StackCard -> StackCard) -> Model -> Model
modStackHead f m =
  case headMay (getStack m) of
    Nothing ->
      m
    Just c ->
      (setStack (f c : (tailSafe (getStack m)))) m

modStackAll :: (StackCard -> StackCard) -> Model -> Model
modStackAll f m = modStack (fmap f) m

-- HOVER CARD.
getHover :: WhichPlayer -> Model -> HoverCardIndex
getHover PlayerA (Model _ _ _ _ _ _ _ _ hoverPA hoverPB _ _ _) = hoverPA
getHover PlayerB (Model _ _ _ _ _ _ _ _ hoverPA hoverPB _ _ _) = hoverPB

setHover :: WhichPlayer -> HoverCardIndex -> Model -> Model
setHover PlayerA hoverPA (Model turn stack handPA handPB deckPA deckPB lifePA lifePB _ hoverPB passes res gen) =
  (Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen)
setHover PlayerB hoverPB (Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA _ passes res gen) =
  (Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen)

-- RESOLVING.
resolveOne :: GameState -> GameState
resolveOne (Playing model@(Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen)) =
  rememberRes model $ lifeGate $ Playing (eff (modStack tailSafe model))
  where
    eff :: Model -> Model
    eff = case headMay stack of
      Nothing -> id
      Just (StackCard p (Card _ _ _ effect)) -> effect p
    lifeGate :: GameState -> GameState
    lifeGate s@(Playing m@(Model _ _ _ _ _ _ lifePA lifePB _ _ _ res _))
      | (lifePA <= 0) && (lifePB <= 0) = Draw gen res
      | lifePB <= 0 = Victory PlayerA gen res
      | lifePA <= 0 = Victory PlayerB gen res
      | otherwise = Playing (maxLifeGate m)
    lifeGate x = x
    maxLifeGate :: Model -> Model
    maxLifeGate m =
      setLife PlayerA (min lifeMax (getLife PlayerA m)) $
        setLife PlayerB (min lifeMax (getLife PlayerB m)) m
resolveOne x = x

resolveAll :: GameState -> GameState
resolveAll state@(Playing model) =
  case null (getStack model) of
    True -> state
    False -> resolveAll $ resolveOne state
resolveAll x = x

rememberRes :: Model -> GameState -> GameState
rememberRes r (Playing m@(Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen)) =
  (Playing (Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes (res ++ [resetResModel r]) gen))
rememberRes r (Victory p gen res) =
  Victory p gen (res ++ [resetResModel r])
rememberRes r (Draw gen res) =
  Draw gen (res ++ [resetResModel r])
rememberRes r x = x

resetRes :: GameState -> GameState
resetRes (Playing m) = Playing (resetResModel m)
resetRes x = x

resetResModel :: Model -> Model
resetResModel (Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen) =
  (Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes [] gen)

-- ACTIONS
hurt :: Life -> WhichPlayer -> Model -> Model
hurt damage PlayerA model =
  modLife (\l -> l - damage) PlayerA model
hurt damage PlayerB model =
  modLife (\l -> l - damage) PlayerB model

heal :: Life -> WhichPlayer -> Model -> Model
heal life PlayerA model =
  modLife (\l -> l + life) PlayerA model
heal life PlayerB model =
  modLife (\l -> l + life) PlayerB model

lifesteal :: Life -> WhichPlayer -> Model -> Model
lifesteal d p m = heal d (otherPlayer p) $ hurt d p m

-- CARDS

cardDagger :: Card
cardDagger = Card "Dagger" "Hurt for 8" "plain-dagger.svg" eff
  where
    eff :: CardEff
    eff p m = hurt 8 (otherPlayer p) m

cardHubris :: Card
cardHubris = Card "Hubris" "Negate all cards to the right" "tower-fall.svg" eff
  where
    eff :: CardEff
    eff p m = setStack [] m

cardFireball :: Card
cardFireball = Card "Fireball" "Hurt for 4 for each card to the right" "fire-ray.svg" eff
  where
    eff :: CardEff
    eff p m = hurt (4 * (length (getStack m))) (otherPlayer p) m

cardBoomerang :: Card
cardBoomerang = Card "Boomerang" "Hurt for 2, return this card to your hand" "boomerang.svg" eff
  where
    eff :: CardEff
    eff p m = modHand (addToHand cardBoomerang) p (hurt 2 (otherPlayer p) m)

cardPotion :: Card
cardPotion = Card "Potion" "Heal for 7" "heart-bottle.svg" eff
  where
    eff :: CardEff
    eff p m = heal 7 p m

cardVampire :: Card
cardVampire = Card "Vampire" "Lifesteal for 5" "fangs.svg" eff
  where
    eff :: CardEff
    eff p m = lifesteal 5 (otherPlayer p) m

cardSuccubus :: Card
cardSuccubus = Card "Succubus" "Lifesteal for 2 for each card to the right" "pretty-fangs.svg" eff
  where
    eff :: CardEff
    eff p m = lifesteal (2 * (length (getStack m))) (otherPlayer p) m

cardReversal :: Card
cardReversal = Card "Reversal" "Reverse the order of cards to the right" "pocket-watch.svg" eff
  where
    eff :: CardEff
    eff p m = modStack reverse m

cardReflect :: Card
cardReflect = Card "Reflect" "All cards to the right change owner" "shield-reflect.svg" eff
  where
    eff :: CardEff
    eff p m = modStackAll reflectEff m
    reflectEff :: StackCard -> StackCard
    reflectEff (StackCard owner (Card name desc img cardEff)) =
      StackCard (otherPlayer owner) (Card name desc img cardEff)

-- cardEcho :: Card
-- cardEcho = Card "Echo" "All cards to the right happen twice" "echo-ripples.svg" eff
--   where
--     eff :: CardEff
--     eff p m = modStackAll echoEff m
--     echoEff :: StackCard -> StackCard
--     echoEff (StackCard owner (Card name desc img cardEff)) =
--       StackCard owner (Card name desc img (echo cardEff))
--     echo :: CardEff -> CardEff
--     echo e = \which -> (e which) . (e which)

cardProphecy :: Card
cardProphecy = Card "Prophecy" "Return all cards to the right to their owner's hand" "crystal-ball.svg" eff
  where
    eff :: CardEff
    eff p m =
      modHand (bounceAll PlayerA (getStack m)) PlayerA $
      modHand (bounceAll PlayerB (getStack m)) PlayerB $
      setStack [] m
    bounceAll :: WhichPlayer -> Stack -> Hand -> Hand
    bounceAll w s h = (fmap getCard (filter (owner w) s)) ++ h
    owner :: WhichPlayer -> StackCard -> Bool
    owner PlayerA (StackCard PlayerA _) = True
    owner PlayerB (StackCard PlayerB _) = True
    owner _ _ = False
    getCard :: StackCard -> Card
    getCard (StackCard _ card) = card


cardSiren :: Card
cardSiren = Card "Siren" "Your opponent gets two cards that hurt them for 8 damage when played" "harpy.svg" eff
  where
    eff :: CardEff
    eff p m = modHand (times 2 (addToHand cardSong)) (otherPlayer p) m
    cardSong :: Card
    cardSong = Card "Siren's Song" "Hurt yourself for 8" "love-song.svg" (hurt 8)


cardSickness :: Card
cardSickness = Card "Sickness" "Make all cards to the right's healing hurt instead" "bleeding-heart.svg" eff
  where
    eff :: CardEff
    eff p m = modStackAll (patchedCard m) m
    patchedCard :: Model -> StackCard -> StackCard
    patchedCard model (StackCard owner (Card name desc img cardEff)) =
      StackCard owner (Card ("Sick " <> name) (desc <> "; afflicted by sickness.") img (patchedEff cardEff))
    patchedEff :: CardEff -> CardEff
    patchedEff eff =
      \w m -> reverseHeal PlayerA m $ reverseHeal PlayerB m $ eff w m
    reverseHeal :: WhichPlayer -> Model -> Model -> Model
    reverseHeal which m1 m2 =
      hurt (max 0 (((getLife which m2) - (getLife which m1)) * 2)) which m2

cardOffering :: Card
cardOffering = Card "Offering" "Discard your hand, then draw two cards" "chalice-drops.svg" eff
  where
    eff :: CardEff
    eff p m = (drawCard p) . (drawCard p) $ setHand p [] m

cardGoatFlute :: Card
cardGoatFlute = Card "Goat Flute" "Both players get two useless goats" "pan-flute.svg" eff
  where
    eff :: CardEff
    eff p m =
      modHand (times 2 (addToHand cardGoat)) p $
        modHand (times 2 (addToHand cardGoat)) (otherPlayer p) m
    cardGoat :: Card
    cardGoat = Card "Goat" "A useless card" "goat.svg" (\_ m -> m)

cardConfound :: Card
cardConfound = Card "Confound" "Shuffle the order of cards to the right" "moebius-star.svg" eff
  where
    eff :: CardEff
    eff p m = modStack (\s -> shuffle' s (length s) (getGen m)) m

cardObscurer :: Card
cardObscurer = Card "Obscurer" "Hurt for 4 and obscure the next card your opponent draws" "orb-wand.svg" eff
  where
    eff :: CardEff
    eff p m = hurt 4 (otherPlayer p) $ modDeckHead obs (otherPlayer p) m
    obs :: Card -> Card
    obs card = Card "???" "An obscured card" "sight-disabled.svg" (\p -> modStack ((:) (StackCard p card)))
