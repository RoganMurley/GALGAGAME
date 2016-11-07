{-# LANGUAGE OverloadedStrings #-}
module GameState where

import Control.Applicative ((<$>))
import Control.Monad (MonadPlus, mplus)
import Data.Aeson (ToJSON(..), (.=), object)
import Data.List (partition)
import Data.Maybe (isJust, maybeToList)
import Data.Text (Text)
import Safe (headMay, tailSafe)
import System.Random (StdGen, split)
import System.Random.Shuffle (shuffle')


data GameState = Waiting StdGen | Playing Model | Victory WhichPlayer | Draw

data Model = Model Turn Stack Hand Hand Deck Deck Life Life Passes StdGen
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
    toJSON model
  toJSON (Victory which) =
    object [
      "victory" .= which
    ]
  toJSON (Draw) =
    object [
      "draw" .= True
    ]

instance ToJSON Model where
  toJSON (Model turn stack handPA handPB deckPA deckPB lifePA lifePB _ _) =
    object
      [
        "playing" .=
          object
            [
              "turn" .= turn
            , "stack" .= stack
            , "handPA" .= handPA
            , "handPB" .= handPB
            , "lifePA" .= lifePA
            , "lifePB" .= lifePB
            ]
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

handMaxLength :: Int
handMaxLength = 6

initModel :: StdGen -> Model
initModel gen = Model PlayerA [] handPA handPB deckPA deckPB initLife initLife NoPass gen
  where
    initLife = 100 :: Life
    (genPA, genPB) = split gen :: (StdGen, StdGen)
    initDeckPA = shuffle' initDeck (length initDeck) genPA :: Deck
    (handPA, deckPA) = splitAt 5 initDeckPA :: (Hand, Deck)
    initDeckPB = shuffle' initDeck (length initDeck) genPB :: Deck
    (handPB, deckPB) = splitAt 5 initDeckPB :: (Hand, Deck)

initDeck :: Deck
initDeck =
  -- DAMAGE
     (replicate 3 cardFireball)
  ++ (replicate 3 cardDagger)
  ++ (replicate 3 cardBoomerang)
  ++ (replicate 3 cardHeal)
  ++ (replicate 3 cardVamp)
  ++ (replicate 3 cardSucc)
  -- CONTROL
  ++ (replicate 2 cardHubris)
  -- ++ (replicate 2 cardBounce)
  ++ (replicate 2 cardReflect)
  ++ (replicate 2 cardMirror)


-- TEMP STUFF.
reverso :: GameState -> GameState
reverso (Playing (Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes gen)) =
  (Playing (Model (otherTurn turn) (stackRev stack) handPB handPA deckPB deckPA lifePB lifePA passes gen))
  where
    stackRev :: Stack -> Stack
    stackRev stack = fmap (\(StackCard p c) -> StackCard (otherPlayer p) c) stack
reverso (Victory which) = Victory (otherPlayer which)
reverso x = x


-- UPDATE

update :: GameCommand -> WhichPlayer -> GameState -> Maybe GameState
update cmd which state =
  case state of
    (Playing model) ->
      case cmd of
        EndTurn ->
          endTurn which model
        PlayCard name ->
          Playing <$> (playCard name which model)
    x ->
      Just x

drawCard :: WhichPlayer -> Model -> Maybe Model
drawCard which model@(Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes gen)
  | (length hand >= handMaxLength) = Nothing
  | otherwise =
    case drawnCard of
      Just card ->
        Just (setDeck which drawnDeck $ setHand which (card : hand) model)
      Nothing ->
        Nothing
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
endTurn :: WhichPlayer -> Model -> Maybe GameState
endTurn which model@(Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes gen)
  | turn /= which = Nothing
  | otherwise =
    case bothPassed of
      True ->
        case resolveAll (Playing model) of
          Playing m ->
            Playing <$> (drawCards $ resetPasses $ swapTurn m)
          s ->
            Just s
      False -> Just $ Playing $ swapTurn $ model
  where
    bothPassed :: Bool
    bothPassed = passes == OnePass
    drawCards :: Model -> Maybe Model
    drawCards m = Just m >>? drawCard PlayerA >>? drawCard PlayerB

-- If a computation fails, ignore it.
(>>?) :: (MonadPlus m) => m a -> (a -> m a) -> m a
(>>?) x f = mplus (x >>= f) x

-- In future, tag cards in hand with a uid and use that.
playCard :: CardName -> WhichPlayer -> Model -> Maybe Model
playCard name which model@(Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes gen)
  | turn /= which = Nothing
  | otherwise =
    card *> (Just (resetPasses $ swapTurn $ setStack ((maybeToList card) ++ stack) $ setHand which newHand model))
  where
    hand :: Hand
    hand = getHand which model
    (matches, misses) = partition (\(Card n _ _ _) -> n == name) hand :: ([Card], [Card])
    newHand :: Hand
    newHand = (tailSafe matches) ++ misses
    card :: Maybe StackCard
    card = (StackCard which) <$> (headMay matches)

swapTurn :: Model -> Model
swapTurn model@(Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes gen) =
  Model (otherTurn turn) stack handPA handPB deckPA deckPB lifePA lifePB (incPasses passes) gen

incPasses :: Passes -> Passes
incPasses NoPass = OnePass
incPasses OnePass = NoPass

resetPasses :: Model -> Model
resetPasses (Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes gen) =
  Model turn stack handPA handPB deckPA deckPB lifePA lifePB NoPass gen

otherTurn :: Turn -> Turn
otherTurn PlayerA = PlayerB
otherTurn PlayerB = PlayerA

otherPlayer :: WhichPlayer -> WhichPlayer
otherPlayer = otherTurn

getHand :: WhichPlayer -> Model -> Hand
getHand PlayerA (Model _ _ handPA handPB _ _ _ _ _ _) = handPA
getHand PlayerB (Model _ _ handPA handPB _ _ _ _ _ _) = handPB

setHand :: WhichPlayer -> Hand -> Model -> Model
setHand PlayerA newHand (Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes gen) =
  Model turn stack newHand handPB deckPA deckPB lifePA lifePB passes gen
setHand PlayerB newHand (Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes gen) =
  Model turn stack handPA newHand deckPA deckPB lifePA lifePB passes gen

getDeck :: WhichPlayer -> Model -> Deck
getDeck PlayerA (Model _ _ _ _ deckPA deckPB _ _ _ _) = deckPA
getDeck PlayerB (Model _ _ _ _ deckPA deckPB _ _ _ _) = deckPB

setDeck :: WhichPlayer -> Deck -> Model -> Model
setDeck PlayerA newDeck (Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes gen) =
  Model turn stack handPA handPB newDeck deckPB lifePA lifePB passes gen
setDeck PlayerB newDeck (Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes gen) =
  Model turn stack handPA handPB deckPA newDeck lifePA lifePB passes gen

getStack :: Model -> Stack
getStack (Model _ stack _ _ _ _ _ _ _ _) = stack

setStack :: Stack -> Model -> Model
setStack newStack (Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes gen) =
  Model turn newStack handPA handPB deckPA deckPB lifePA lifePB passes gen

resolveOne :: GameState -> GameState
resolveOne (Playing model@(Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes gen)) =
  lifeGate $ Playing (eff (setStack (tailSafe stack) model))
  where
    eff :: Model -> Model
    eff = case headMay stack of
      Nothing -> id
      Just (StackCard p (Card _ _ _ effect)) -> effect p
    lifeGate :: GameState -> GameState
    lifeGate s@(Playing (Model _ _ _ _ _ _ lifePA lifePB _ _))
      | (lifePA <= 0) && (lifePB <= 0) = Draw
      | lifePB <= 0 = Victory PlayerA
      | lifePA <= 0 = Victory PlayerB
      | otherwise = s
    lifeGate x = x
resolveOne x = x

resolveAll :: GameState -> GameState
resolveAll state@(Playing model) =
  case stackEmpty of
    True -> state
    False -> resolveAll $ resolveOne state
  where
    stackEmpty :: Bool
    stackEmpty = null (getStack model)
resolveAll x = x

hurt :: Life -> WhichPlayer -> Model -> Model
hurt damage PlayerA (Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes gen) =
  Model turn stack handPA handPB deckPA deckPB (lifePA - damage) lifePB passes gen
hurt damage PlayerB (Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes gen) =
  Model turn stack handPA handPB deckPA deckPB lifePA (lifePB - damage) passes gen

-- CARDS

cardDagger :: Card
cardDagger = Card "Dagger" "Hurt for 200" "plain-dagger.svg" eff
  where
    eff :: CardEff
    eff p m = hurt 200 (otherPlayer p) m

cardHubris :: Card
cardHubris = Card "Hubris" "Negate whole combo" "tower-fall.svg" eff
  where
    eff :: CardEff
    eff p m = setStack [] m

cardFireball :: Card
cardFireball = Card "Fireball" "Hurt for 80 per combo" "fire-ray.svg" eff
  where
    eff :: CardEff
    eff p m = hurt (80 * ((length (getStack m)) + 1)) (otherPlayer p) m

cardBoomerang :: Card
cardBoomerang = Card "Boomerang" "Hurt for 50, get back at end of combo" "boomerang.svg" eff
  where
    eff :: CardEff
    eff p m = setStack ((getStack m) ++ [StackCard p cardCatch]) (hurt 50 (otherPlayer p) m)
    cardCatch :: Card
    cardCatch = Card "Catch Boomerang" "Get boomerang back" "hand.svg" catchEff
    catchEff :: CardEff
    catchEff p m = setHand p (cardBoomerang : (getHand p m)) m

cardHeal :: Card
cardHeal = Card "Elixir" "Heal for 100" "heart-bottle.svg" eff
  where
    eff :: CardEff
    eff p m = hurt (-100) p m

cardVamp :: Card
cardVamp = Card "Vampire" "Lifesteal for 100" "fangs.svg" eff
  where
    eff :: CardEff
    eff p m = hurt 100 (otherPlayer p) $ hurt (-50) p m

cardSucc :: Card
cardSucc = Card "Succubus" "Lifesteal for 50 per combo" "pretty-fangs.svg" eff
  where
    eff :: CardEff
    eff p m =
      hurt (50 * ((length (getStack m)) + 1)) (otherPlayer p) $
        hurt (-25 * ((length (getStack m)) + 1)) p m

cardBounce :: Card
cardBounce = Card "Whence" "Return top of combo to its owner" "thor-fist.svg" eff
  where
    eff :: CardEff
    eff p m =
      case headMay (getStack m) of
        Nothing ->
          m
        Just (StackCard owner card) ->
          setHand owner (card : (getHand owner m)) $ (setStack (tailSafe (getStack m))) m

cardReflect :: Card
cardReflect = Card "Reflect" "Change the top of the combo's owner" "shield-reflect.svg" eff
  where
    eff :: CardEff
    eff p m =
      case headMay (getStack m) of
        Nothing ->
          m
        Just (StackCard owner card) ->
          (setStack ( (StackCard (otherPlayer owner) card) : (tailSafe (getStack m)))) m


cardMirror :: Card
cardMirror = Card "Mirror" "Reverse the combo order" "mirror-mirror.svg" eff
  where
    eff :: CardEff
    eff p m = setStack (reverse $ getStack m) m
