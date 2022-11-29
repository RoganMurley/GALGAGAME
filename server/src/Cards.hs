module Cards where

import Card (Aspect (..), Card (..), Status (..), Suit (..), addRelated, addStatus, cardName, hasStatus, newCard)
import CardAnim (Hurt (..), TimeModifier (..))
import Control.Monad (replicateM_, when)
import qualified DSL.Alpha as Alpha
import DSL.Beta
import qualified DSL.Beta as Beta
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import HandCard (HandCard (..), anyCard, isRevealed)
import Player (other)
import Safe (headMay)
import Stack (diasporaFromStack, diasporaLength)
import qualified Stack
import StackCard (StackCard (..), cardMap, changeOwner, isOwner)
import Transmutation (Transmutation (..), transmuteToCard)
import Util (many, randomBetween, shuffle)

-- FIRE
fireSword :: Card
fireSword =
  newCard
    Fire
    Sword
    "Hurt for 7"
    $ \w -> hurt 7 (other w) Slash

fireWand :: Card
fireWand =
  newCard
    Fire
    Wand
    "Hurt for 5 for each other card\non the wheel"
    $ \w -> do
      len <- diasporaLength <$> getStack
      hurt (len * 5) (other w) Slash

fireCup :: Card
fireCup =
  newCard
    Fire
    Cup
    "Discard your hand, then draw 2"
    $ \w -> do
      discardHand w (\_ _ -> True)
      draw w w (TimeModifierOutQuint 1)
      draw w w (TimeModifierOutQuint 1)

fireCoin :: Card
fireCoin =
  newCard
    Fire
    Coin
    "Shuffle the order of all other\ncards on the wheel"
    $ \_ -> do
      confound
      Beta.null

-- WATER
waterSword :: Card
waterSword =
  newCard
    Water
    Sword
    "Hurt for 3, return this card\nto hand"
    $ \w -> do
      hurt 3 (other w) Slash
      bounce (\i _ -> i == 0) (TimeModifierOutQuad 0.4)

waterWand :: Card
waterWand =
  newCard
    Water
    Wand
    "Hurt for 3 for each other card\non the wheel, then draw 1"
    $ \w -> do
      len <- diasporaLength <$> getStack
      hurt (len * 3) (other w) Slash
      draw w w (TimeModifierOutQuad 1)

waterCup :: Card
waterCup =
  newCard
    Water
    Cup
    "Heal for 8"
    $ \w -> heal 8 w

waterCoin :: Card
waterCoin =
  newCard
    Water
    Coin
    "Discard cards in the\nnext 3 wheel sockets"
    $ \_ -> do
      discardStack (\i _ -> (i > 0) && (i < 4))

-- ANGEL
angelSword :: Card
angelSword =
  newCard
    Angel
    Sword
    "Hurt for 8"
    $ \w -> hurt 8 (other w) Slash

angelWand :: Card
angelWand =
  newCard
    Angel
    Wand
    "Hurt for 4 for each other card\non the wheel"
    $ \w -> do
      len <- diasporaLength <$> getStack
      hurt (len * 4) (other w) Slash

angelCup :: Card
angelCup =
  newCard
    Angel
    Cup
    "Heal for 2, return this card\nto hand"
    $ \w -> do
      heal 2 w
      bounce (\i _ -> i == 0) (TimeModifierOutQuad 0.4)

angelCoin :: Card
angelCoin =
  newCard
    Angel
    Coin
    "Return all of your cards on the\nwheel to hand"
    $ \w -> bounce (\i (StackCard o _) -> i > 0 && w == o) (TimeModifierOutQuint 1)

-- VOID
voidSword :: Card
voidSword =
  newCard
    Void
    Sword
    "Hurt for 6"
    $ \w -> hurt 6 (other w) Slash

voidWand :: Card
voidWand =
  newCard
    Void
    Wand
    "Discard your hand, then hurt\nfor 4 for each card\ndiscarded"
    $ \w -> do
      handSize <- length <$> getHand w
      discardHand w (\_ _ -> True)
      hurt (4 * handSize) (other w) Slash

voidCup :: Card
voidCup =
  newCard
    Void
    Cup
    "Become a copy of an unrevealed\ncard in your hand"
    $ \w -> do
      gen <- getGen
      hand <- getHand w
      stack <- getStack
      let mCopyCard = headMay . filter (\(_, c) -> not $ isRevealed c) . shuffle gen $ zip [0 ..] hand
      case mCopyCard of
        Just (index, copyCard) ->
          case Stack.get stack 0 of
            Just selfCard -> do
              transmuteActive (\_ -> Just $ transmuteToCard (anyCard copyCard) selfCard)
              raw $ Alpha.reveal w (\i _ -> i == index)
              Beta.null
            Nothing ->
              return ()
        Nothing ->
          return ()

voidCoin :: Card
voidCoin =
  newCard
    Void
    Coin
    "Discard all cards on the wheel"
    $ \_ -> discardStack (\i _ -> i > 0)

-- DUALITY
dualitySword :: Card
dualitySword =
  newCard
    Duality
    Sword
    "Hurt for 9"
    $ \w -> hurt 9 (other w) Slash

dualityWand :: Card
dualityWand =
  newCard
    Duality
    Wand
    "Hurt weakest player for 15"
    $ \w -> do
      let dmg = 15
      paLife <- getLife w
      pbLife <- getLife (other w)
      when (paLife < pbLife) (hurt dmg w Curse)
      when (paLife > pbLife) (hurt dmg (other w) Curse)
      when (paLife == pbLife) Beta.null

dualityCup :: Card
dualityCup =
  newCard
    Duality
    Cup
    "Heal weakest player for 15"
    $ \w -> do
      let mag = 15
      paLife <- getLife w
      pbLife <- getLife (other w)
      when (paLife < pbLife) (heal mag w)
      when (paLife > pbLife) (heal mag (other w))
      when (paLife == pbLife) Beta.null

dualityCoin :: Card
dualityCoin =
  newCard
    Duality
    Coin
    "Change next card's owner\nto weakest player"
    $ \w -> do
      paLife <- getLife w
      pbLife <- getLife (other w)
      when (paLife > pbLife) (transmuteHead (\(StackCard _ c) -> StackCard (other w) c))
      when (paLife < pbLife) (transmuteHead (\(StackCard _ c) -> StackCard w c))
      when (paLife == pbLife) Beta.null

-- SHROOM
shroomSword :: Card
shroomSword =
  newCard
    Shroom
    Sword
    "Lifesteal for 5"
    $ \w -> lifesteal 5 (other w)

shroomWand :: Card
shroomWand =
  newCard
    Shroom
    Wand
    "Lifesteal for 3 for each other card\non the wheel"
    $ \w -> do
      len <- diasporaLength <$> getStack
      lifesteal (len * 3) (other w)

shroomCup :: Card
shroomCup =
  addRelated strangeSpore $
    newCard
      Shroom
      Cup
      "Give them 2 STRANGE SPOREs"
      $ \w -> do
        addToHand (other w) (KnownHandCard strangeSpore)
        addToHand (other w) (KnownHandCard strangeSpore)

strangeSpore :: Card
strangeSpore =
  newCard
    Strange
    (OtherSuit "SPORE")
    "Hurt yourself for 4"
    $ \w -> do
      hurt 4 w Bite

shroomCoin :: Card
shroomCoin =
  newCard
    Shroom
    Coin
    "Reverse the order of all other\ncards on the wheel"
    $ const reversal

-- BLOOD
bloodSword :: Card
bloodSword =
  newCard
    Blood
    Sword
    "Pay 4 life to hurt for 12"
    $ \w -> do
      payLife 4 w
      hurt 12 (other w) Slash

bloodWand :: Card
bloodWand =
  newCard
    Blood
    Wand
    "Pay half your life to hurt for\nhalf your life"
    $ \w -> do
      l <- getLife w
      let dmg = l `quot` 2
      payLife dmg w
      hurt dmg (other w) Slash

bloodCup :: Card
bloodCup =
  newCard
    Blood
    Cup
    "Pay 4 life to draw 2"
    $ \w -> do
      payLife 4 w
      draw w w (TimeModifierOutQuint 1)
      draw w w (TimeModifierOutQuint 1)

bloodCoin :: Card
bloodCoin =
  newCard
    Blood
    Coin
    "Pay half your life to move card\nin next socket to your hand"
    $ \w -> do
      l <- getLife w
      payLife (l `quot` 2) w
      transmuteHead (\(StackCard _ c) -> StackCard w c)
      bounce (\i _ -> i == 1) (TimeModifierOutQuad 0.4)

-- EYE
eyeSword :: Card
eyeSword =
  newCard
    Eye
    Sword
    "Hurt for 6 and reveal\na card in their hand"
    $ \w -> do
      hurt 6 (other w) Slash
      revealRandomCard (other w)

eyeWand :: Card
eyeWand =
  newCard
    Eye
    Wand
    "Reveal a card in their hand, then\nhurt for 7 for each revealed\ncard in their hand"
    $ \w -> do
      revealRandomCard (other w)
      hand <- getHand (other w)
      let count = length $ filter isRevealed hand
      hurt (count * 7) (other w) Slash

eyeCup :: Card
eyeCup =
  newCard
    Eye
    Cup
    "Reveal a card in their hand, then\ndraw for each revealed\ncard in their hand"
    $ \w -> do
      revealRandomCard (other w)
      hand <- getHand (other w)
      let count = length $ filter isRevealed hand
      many count $ draw w w (TimeModifierOutQuint 1)

eyeCoin :: Card
eyeCoin =
  newCard
    Eye
    Coin
    "Return all cards on the wheel to hand"
    $ \_ -> bounce (\i _ -> i > 0) (TimeModifierOutQuint 1)

-- MIRROR
mirrorSword :: Card
mirrorSword =
  newCard
    Mirror
    Sword
    "Hurt for 7"
    $ \w -> do
      hurt 7 (other w) Slash

mirrorWand :: Card
mirrorWand =
  newCard
    Mirror
    Wand
    "Hurt for 3 for each card in your hand"
    $ \w -> do
      len <- length <$> getHand w
      hurt (len * 3) (other w) Slash

mirrorCup :: Card
mirrorCup =
  newCard
    Mirror
    Cup
    "Double the number of times card\nin next socket activates"
    $ \_ -> do
      transmuteHead (cardMap (addStatus StatusEcho))

mirrorCoin :: Card
mirrorCoin =
  newCard
    Mirror
    Coin
    "Change the owner of all cards\non the wheel"
    $ \_ ->
      transmute $
        \i stackCard -> if i > 0 then Just $ Transmutation stackCard (changeOwner stackCard) else Nothing

-- GOLD
goldSword :: Card
goldSword =
  newCard
    Gold
    Sword
    "Hurt for 6"
    $ \w -> hurt 6 (other w) Slash

goldWand :: Card
goldWand =
  newCard
    Gold
    Wand
    "Hurt for 3 for each card\nin their hand"
    $ \w -> do
      len <- length <$> getHand (other w)
      hurt (len * 3) (other w) Slash

goldCup :: Card
goldCup =
  newCard
    Gold
    Cup
    "Heal for 10"
    $ heal 10

goldCoin :: Card
goldCoin =
  addRelated strangeSubstance $
    newCard
      Gold
      Coin
      "Change card in next socket\nto STRANGE SUBSTANCE"
      $ \_ -> transmuteHead (transmuteToCard strangeSubstance)

strangeSubstance :: Card
strangeSubstance =
  newCard
    Strange
    (OtherSuit "SUBSTANCE")
    "Draw 2"
    $ \w -> do
      draw w w (TimeModifierOutQuint 1)
      draw w w (TimeModifierOutQuint 1)

-- Clay
claySword :: Card
claySword =
  newCard
    Clay
    Sword
    "Hurt for 7, then all CLAY cards on\nthe wheel become SWORDs"
    $ \w -> do
      hurt 7 (other w) Slash
      transmute $
        \_ stackCard ->
          case stackCard of
            StackCard _ Card {card_aspect = Clay} ->
              Just $ Transmutation stackCard (transmuteToCard claySword stackCard)
            _ ->
              Nothing

clayWand :: Card
clayWand =
  newCard
    Clay
    Wand
    "Hurt for 3 for each card on the wheel,\nthen all CLAY cards on the wheel\nbecome WANDs"
    $ \w -> do
      len <- diasporaLength <$> getStack
      hurt (len * 3) (other w) Slash
      transmute $
        \_ stackCard ->
          case stackCard of
            StackCard _ Card {card_aspect = Clay} ->
              Just $ Transmutation stackCard (transmuteToCard clayWand stackCard)
            _ ->
              Nothing

clayCup :: Card
clayCup =
  newCard
    Clay
    Cup
    "Heal for 8, then all CLAY cards on\nthe wheel become CUPs"
    $ \w -> do
      heal 8 w
      transmute $
        \_ stackCard ->
          case stackCard of
            StackCard _ Card {card_aspect = Clay} ->
              Just $ Transmutation stackCard (transmuteToCard clayCup stackCard)
            _ ->
              Nothing

clayCoin :: Card
clayCoin =
  newCard
    Clay
    Coin
    "The next card becomes a CLAY card"
    $ \_ -> do
      transmuteHead $
        \stackCard ->
          let targetCard :: Card
              targetCard =
                case card_suit . stackcard_card $ stackCard of
                  Sword ->
                    claySword
                  Wand ->
                    clayWand
                  Cup ->
                    clayCup
                  Coin ->
                    clayCoin
                  OtherSuit _ ->
                    strangeSnag
           in transmuteToCard targetCard stackCard

-- FEVER
feverSword :: Card
feverSword =
  newCard
    Fever
    Sword
    "Hurt for 8"
    $ \w -> hurt 8 (other w) Slash

feverWand :: Card
feverWand =
  newCard
    Fever
    Wand
    "Hurt for 15 then heal\nthem for 5"
    $ \w -> do
      hurt 15 (other w) Slash
      heal 5 (other w)

feverCup :: Card
feverCup =
  newCard
    Fever
    Cup
    "Healing becomes hurting for all\nother cards on the wheel"
    $ \_ ->
      transmute
        ( \i sc ->
            if i > 0
              then Just (Transmutation sc (cardMap (addStatus StatusBlighted) sc))
              else Nothing
        )

feverCoin :: Card
feverCoin =
  addRelated strangeSleep $
    newCard
      Fever
      Coin
      "Change card in next socket to\nSTRANGE SLEEP"
      $ \_ ->
        transmuteHead (transmuteToCard strangeSleep)

strangeSleep :: Card
strangeSleep =
  newCard
    Strange
    (OtherSuit "SLEEP")
    "Heal for 13"
    $ \w -> do
      heal 13 w

-- GLASS
glassSword :: Card
glassSword =
  addStatus StatusFragile $
    newCard
      Glass
      Sword
      "Hurt for 9.\nThis card is fragile."
      $ \w -> hurt 9 (other w) Slash

glassWand :: Card
glassWand =
  addStatus StatusFragile $
    newCard
      Glass
      Wand
      "Hurt for 7 for each fragile\ncard on the wheel.\nThis card is fragile."
      $ \w -> do
        diaspora <- diasporaFromStack <$> getStack
        let len = length $ filter (\(_, c) -> hasStatus StatusFragile (stackcard_card c)) diaspora
        hurt (len * 7) (other w) Slash

glassCup :: Card
glassCup =
  addStatus StatusFragile $
    newCard
      Glass
      Cup
      "All other cards on the wheel\nbecome fragile. This card\nstarts fragile."
      $ \_ ->
        transmute
          ( \i sc ->
              if i > 0 && not (hasStatus StatusFragile $ stackcard_card sc)
                then Just (Transmutation sc (cardMap (addStatus StatusFragile) sc))
                else Nothing
          )

glassCoin :: Card
glassCoin =
  addStatus StatusFragile $
    newCard
      Glass
      Coin
      "Discard all their cards on the wheel.\nThis card is fragile."
      $ \w ->
        discardStack (\i sc -> (i > 0) && isOwner (other w) sc)

-- PLASTIC
plasticSword :: Card
plasticSword =
  newCard
    Plastic
    Sword
    "Hurt for 7"
    $ \w -> hurt 7 (other w) Slash

plasticWand :: Card
plasticWand =
  newCard
    Plastic
    Wand
    "Hurt for 8 for each PLASTIC WAND\non the wheel"
    $ \w -> do
      stack <- getStack
      let cards = fmap (\(_, sc) -> stackcard_card sc) (Stack.diasporaFromStack stack)
      let mag = length $ filter (\Card {card_aspect, card_suit} -> (card_aspect == Plastic) && (card_suit == Wand)) cards
      hurt (8 * mag) (other w) Slash

plasticCup :: Card
plasticCup =
  newCard
    Plastic
    Cup
    "Draw 2 copies of card in next socket "
    $ \w -> do
      stack <- getStack
      let mNextStackCard = Stack.get stack 1
      case mNextStackCard of
        Just nextStackCard -> do
          addToHand w (KnownHandCard $ stackcard_card nextStackCard)
          addToHand w (KnownHandCard $ stackcard_card nextStackCard)
        Nothing ->
          return ()

plasticCoin :: Card
plasticCoin =
  newCard
    Plastic
    Coin
    "All other cards on the wheel become\na copy of the card in next socket"
    $ \_ -> do
      stack <- getStack
      let mNextStackCard = Stack.get stack 1
      case mNextStackCard of
        Just nextStackCard ->
          transmute $
            \i stackCard ->
              if i > 0
                then Just $ Transmutation stackCard (transmuteToCard (stackcard_card nextStackCard) stackCard)
                else Nothing
        Nothing ->
          return ()

-- Devil
devilSword :: Card
devilSword =
  newCard
    Devil
    Sword
    "Hurt for 4, then hurt for 4 again"
    $ \w -> do
      hurt 4 (other w) Slash
      hurt 4 (other w) Slash

devilWand :: Card
devilWand =
  newCard
    Devil
    Wand
    "Hurt for 3 up to 5 times"
    $ \w -> do
      gen <- getGen
      let times = randomBetween gen 1 5
      unknownDamage
      many times (hurt 3 (other w) Slash)

devilCup :: Card
devilCup =
  newCard
    Devil
    Cup
    "All other cards on the wheel\nget +4 damage"
    $ \_ ->
      transmute
        ( \i sc ->
            if i > 0
              then Just (Transmutation sc (cardMap (addStatus (StatusBonusDamage 4)) sc))
              else Nothing
        )

devilCoin :: Card
devilCoin =
  newCard
    Devil
    Coin
    "Move card in next socket\nto previous socket"
    $ \_ ->
      moveStack
        (\i _ -> if i == 1 then Just (-1) else Nothing)
        (TimeModifierOutQuint 500)

-- devilCoin :: Card
-- devilCoin =
--   newCard
--     Devil
--     Coin
--     "Card in next socket becomes a WAND"
--     $ \_ ->
--       transmuteHead $
--         \stackCard ->
--           let aspect = card_aspect . stackcard_card $ stackCard :: Aspect
--               targetCard = getCard aspect Wand :: Card
--            in transmuteToCard targetCard stackCard

-- Other cards
getEndCard :: Int -> Card
getEndCard noDraws
  | noDraws > 10 = strangeStart
  | otherwise = strangeStop noDraws

strangeStop :: Int -> Card
strangeStop noDraws =
  let dmg = 2 ^ noDraws :: Int
   in newCard
        Strange
        (OtherSuit "STOP")
        ("You're out of cards,\nhurt yourself for " <> cs (show dmg))
        $ \w -> hurt dmg w Slash

strangeStart :: Card
strangeStart =
  newCard
    Strange
    (OtherSuit "START")
    "The cycle continues..."
    $ \w -> do
      gen <- getGen
      let deckA = shuffle gen allCards
      let deckB = shuffle gen deckA
      raw
        ( do
            Alpha.setDeck w deckA
            Alpha.setDeck (other w) deckB
        )
      discardHand w (\_ _ -> True)
      discardHand (other w) (\_ _ -> True)
      replicateM_ 5 (Beta.draw w w (TimeModifierOutQuint 1))
      replicateM_ 5 (Beta.draw (other w) (other w) (TimeModifierOutQuint 1))

strangeSnag :: Card
strangeSnag =
  newCard
    Strange
    (OtherSuit "SNAG")
    "You feel like something\nis missing..."
    $ \w -> do
      Beta.null
      Beta.null
      Beta.null
      Beta.draw w w (TimeModifierOutQuint 1)
      discardStack (\i _ -> i == 0)
      raw $ Alpha.setHold True

-- Aggregations
swords :: [Card]
swords =
  [ fireSword,
    angelSword,
    shroomSword,
    bloodSword,
    mirrorSword,
    dualitySword,
    goldSword,
    claySword,
    waterSword,
    feverSword,
    voidSword,
    eyeSword,
    glassSword,
    plasticSword,
    devilSword
  ]

wands :: [Card]
wands =
  [ fireWand,
    angelWand,
    shroomWand,
    bloodWand,
    mirrorWand,
    dualityWand,
    goldWand,
    clayWand,
    waterWand,
    feverWand,
    voidWand,
    eyeWand,
    glassWand,
    plasticWand,
    devilWand
  ]

cups :: [Card]
cups =
  [ fireCup,
    angelCup,
    shroomCup,
    bloodCup,
    mirrorCup,
    dualityCup,
    goldCup,
    clayCup,
    waterCup,
    feverCup,
    voidCup,
    eyeCup,
    glassCup,
    plasticCup,
    devilWand
  ]

coins :: [Card]
coins =
  [ fireCoin,
    angelCoin,
    shroomCoin,
    bloodCoin,
    mirrorCoin,
    dualityCoin,
    goldCoin,
    clayCoin,
    waterCoin,
    feverCoin,
    voidCoin,
    eyeCoin,
    glassCoin,
    plasticCoin,
    devilCoin
  ]

others :: [Card]
others =
  [ strangeSpore,
    strangeStart,
    strangeSubstance,
    strangeSleep,
    strangeSnag
  ]

allCards :: [Card]
allCards = swords ++ wands ++ cups ++ coins ++ others

cardsByName :: Map Text Card
cardsByName = Map.fromList $ fmap (\card -> (cardName (card_aspect card) (card_suit card), card)) allCards

cardsByAspect :: Map Aspect [Card]
cardsByAspect = Map.fromListWith (++) $ fmap (\card -> (card_aspect card, [card])) allCards

getAspectCards :: Aspect -> [Card]
getAspectCards aspect = fromMaybe [] $ Map.lookup aspect cardsByAspect

cardsBySuit :: Map Suit [Card]
cardsBySuit = Map.fromListWith (++) $ fmap (\card -> (card_suit card, [card])) allCards

getSuitCards :: Suit -> [Card]
getSuitCards suit = fromMaybe [] $ Map.lookup suit cardsBySuit

getCard :: Aspect -> Suit -> Card
getCard aspect suit = fromMaybe strangeSnag mCard
  where
    mCard :: Maybe Card
    mCard = List.find (\Card {card_suit} -> card_suit == suit) aspectCards
    aspectCards :: [Card]
    aspectCards = fromMaybe [] $ Map.lookup aspect cardsByAspect
