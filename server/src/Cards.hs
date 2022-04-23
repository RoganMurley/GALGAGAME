module Cards where

import Card (Aspect (..), Card (..), Status (..), Suit (..), addStatus, cardName, newCard)
import CardAnim (Hurt (..), TimeModifier (..))
import Control.Monad (when)
import qualified DSL.Alpha as Alpha
import DSL.Beta
import qualified DSL.Beta as Beta
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Ease
import HandCard (HandCard (..), anyCard, isRevealed)
import Player (other)
import Safe (headMay)
import Stack (diasporaLength)
import qualified Stack
import StackCard (StackCard (..), cardMap, changeOwner)
import Transmutation (Transmutation (..), transmuteToCard)
import Util (many, manyIndexed, shuffle)

-- Blaze
blazeSword :: Card
blazeSword =
  newCard
    Blaze
    Sword
    "Hurt for 7"
    $ \w -> hurt 7 (other w) Slash

blazeWand :: Card
blazeWand =
  newCard
    Blaze
    Wand
    "Hurt for 5 for each other card\non the wheel"
    $ \w -> do
      len <- diasporaLength <$> getStack
      hurt (len * 5) (other w) Slash

blazeGrail :: Card
blazeGrail =
  newCard
    Blaze
    Grail
    "Discard your hand, then draw 2"
    $ \w -> do
      discardHand w (\_ _ -> True)
      draw w w (TimeModifierOutQuint 1)
      draw w w (TimeModifierOutQuint 1)

blazeCoin :: Card
blazeCoin =
  newCard
    Blaze
    Coin
    "Shuffle the order of all other\ncards on the wheel"
    $ const confound

-- Tide
tideSword :: Card
tideSword =
  newCard
    Tide
    Sword
    "Hurt for 3, return this card\nto hand"
    $ \w -> do
      hurt 3 (other w) Slash
      bounce (\i _ -> i == 0) (TimeModifierOutQuint 0.6)

tideWand :: Card
tideWand =
  newCard
    Tide
    Wand
    "Hurt for 3 for each other card\non the wheel, then draw 1"
    $ \w -> do
      len <- diasporaLength <$> getStack
      hurt (len * 3) (other w) Slash
      draw w w (TimeModifierLinear 1)

tideGrail :: Card
tideGrail =
  newCard
    Tide
    Grail
    "Heal for 8"
    $ \w -> heal 8 w

tideCoin :: Card
tideCoin =
  newCard
    Tide
    Coin
    "Discard cards in the\nnext 3 sockets"
    $ \_ -> do
      discardStack (\i _ -> (i > 0) && (i < 4))

-- Heaven
heavenSword :: Card
heavenSword =
  newCard
    Heaven
    Sword
    "Hurt for 8"
    $ \w -> hurt 8 (other w) Slash

heavenWand :: Card
heavenWand =
  newCard
    Heaven
    Wand
    "Hurt for 4 for each other card\non the wheel"
    $ \w -> do
      len <- diasporaLength <$> getStack
      hurt (len * 4) (other w) Slash

heavenGrail :: Card
heavenGrail =
  newCard
    Heaven
    Grail
    "Heal for 2, return this card\nto hand"
    $ \w -> do
      heal 2 w
      bounce (\i _ -> i == 0) (TimeModifierOutQuint 0.6)

heavenCoin :: Card
heavenCoin =
  newCard
    Heaven
    Coin
    "Return all of your cards on the\nwheel to hand"
    $ \w -> bounce (\i (StackCard o _) -> i > 0 && w == o) (TimeModifierOutQuint 1)

-- Empty
emptySword :: Card
emptySword =
  newCard
    Empty
    Sword
    "Hurt for 6"
    $ \w -> hurt 6 (other w) Slash

emptyWand :: Card
emptyWand =
  newCard
    Empty
    Wand
    "Discard your hand, then hurt\nfor 4 for each card\ndiscarded"
    $ \w -> do
      handSize <- length <$> getHand w
      discardHand w (\_ _ -> True)
      hurt (4 * handSize) (other w) Slash

emptyGrail :: Card
emptyGrail =
  newCard
    Empty
    Grail
    "Become a copy of a random card\nin your hand"
    $ \w -> do
      gen <- getGen
      hand <- getHand w
      stack <- getStack
      let mCopyCard = headMay . shuffle gen $ zip [0 ..] hand
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

emptyCoin :: Card
emptyCoin =
  newCard
    Empty
    Coin
    "Discard all cards on the wheel"
    $ \_ -> discardStack (\i _ -> i > 0)

-- Duality
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

dualityGrail :: Card
dualityGrail =
  newCard
    Duality
    Grail
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

-- Shroom
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

shroomGrail :: Card
shroomGrail =
  newCard
    Shroom
    Grail
    "Give them 2 STRANGE SPOREs\n(Hurt yourself for 4)"
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

-- Blood
bloodSword :: Card
bloodSword =
  newCard
    Blood
    Sword
    "Pay 4 life to hurt for 12"
    $ \w -> do
      hurt 4 w Slash
      hurt 12 (other w) Slash

bloodWand :: Card
bloodWand =
  newCard
    Blood
    Wand
    "Both player's life becomes that\nof the weakest"
    $ \w -> do
      lifePa <- getLife w
      lifePb <- getLife (other w)
      if (lifePa > lifePb)
        then (hurt (lifePa - lifePb) w Slash)
        else (hurt (lifePb - lifePa) (other w) Slash)

bloodGrail :: Card
bloodGrail =
  newCard
    Blood
    Grail
    "Pay 4 life to draw 3"
    $ \w -> do
      hurt 4 w Slash
      draw w w (TimeModifierOutQuint 1)
      draw w w (TimeModifierOutQuint 1)
      draw w w (TimeModifierOutQuint 1)

bloodCoin :: Card
bloodCoin =
  newCard
    Blood
    Coin
    "Pay half your life to negate\ncard in next socket"
    $ \w -> do
      l <- getLife w
      hurt (l `quot` 2) w Slash
      raw $ Alpha.modStackHead $ cardMap (addStatus StatusNegate)
      Beta.null

-- SEER
seerSword :: Card
seerSword =
  newCard
    Seer
    Sword
    "Hurt for 6 and reveal\na card in their hand"
    $ \w -> do
      hurt 6 (other w) Slash
      revealRandomCard (other w)

seerWand :: Card
seerWand =
  newCard
    Seer
    Wand
    "Reveal a card in their hand, then\nhurt for 7 for each revealed\ncard in their hand"
    $ \w -> do
      revealRandomCard (other w)
      hand <- getHand (other w)
      let count = length $ filter isRevealed hand
      hurt (count * 7) (other w) Slash

seerGrail :: Card
seerGrail =
  newCard
    Seer
    Grail
    "Reveal a card in their hand, then\ndraw for each revealed\ncard in their hand"
    $ \w -> do
      revealRandomCard (other w)
      hand <- getHand (other w)
      let count = length $ filter isRevealed hand
      many count $ draw w w (TimeModifierOutQuint 1)

seerCoin :: Card
seerCoin =
  newCard
    Seer
    Coin
    "Return all cards on the wheel to hand"
    $ \_ -> bounce (\i _ -> i > 0) (TimeModifierOutQuint 1)

-- Mirror
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

mirrorGrail :: Card
mirrorGrail =
  newCard
    Mirror
    Grail
    "Double the number of times card\nin next socket activates"
    $ \_ -> do
      raw $ Alpha.modStackHead $ cardMap (addStatus StatusEcho)
      Beta.null

mirrorCoin :: Card
mirrorCoin =
  newCard
    Mirror
    Coin
    "Change the owner of all cards\non the wheel"
    $ \_ ->
      transmute $
        \i stackCard -> if i > 0 then Just $ Transmutation stackCard (changeOwner stackCard) else Nothing

-- Alchemy
alchemySword :: Card
alchemySword =
  newCard
    Alchemy
    Sword
    "Hurt for 6"
    $ \w -> hurt 6 (other w) Slash

alchemyWand :: Card
alchemyWand =
  newCard
    Alchemy
    Wand
    "Hurt for 3 for each card\nin their hand"
    $ \w -> do
      len <- length <$> getHand (other w)
      hurt (len * 3) (other w) Slash

alchemyGrail :: Card
alchemyGrail =
  newCard
    Alchemy
    Grail
    "Heal for 10"
    $ heal 10

alchemyCoin :: Card
alchemyCoin =
  newCard
    Alchemy
    Coin
    "Change card in next socket\nto STRANGE GOLD\n(Draw 2)"
    $ \_ -> transmuteHead (transmuteToCard strangeGold)

strangeGold :: Card
strangeGold =
  newCard
    Strange
    (OtherSuit "GOLD")
    "Draw 2"
    $ \w -> do
      draw w w (TimeModifierOutQuint 1)
      draw w w (TimeModifierOutQuint 1)

-- Crown
crownSword :: Card
crownSword =
  newCard
    Crown
    Sword
    "Hurt for 10"
    $ \w -> hurt 10 (other w) Slash

crownWand :: Card
crownWand =
  newCard
    Crown
    Wand
    "Discard your hand, then hurt\nfor 5 for each card discarded"
    $ \w -> do
      handSize <- length <$> getHand w
      discardHand w (\_ _ -> True)
      hurt (5 * handSize) (other w) Slash

crownGrail :: Card
crownGrail =
  newCard
    Crown
    Grail
    "Your opponent is forced to\nplay a random card"
    $ \w -> do
      gen <- getGen
      hand <- getHand (other w)
      let mCard = headMay . shuffle gen $ zip [0 ..] hand
      case mCard of
        Just (index, c) -> do
          Beta.play (other w) c index
          Beta.windup
        Nothing ->
          Beta.null

crownCoin :: Card
crownCoin =
  newCard
    Crown
    Coin
    "Discard next card for each\ncard in your hand"
    $ \w -> do
      handLen <- length <$> getHand w
      discardStack $ \i _ -> (i /= 0) && (i < handLen + 1)

-- Morph
morphSword :: Card
morphSword =
  newCard
    Morph
    Sword
    "Hurt for 7, then all MORPH cards on\nthe wheel become SWORDs"
    $ \w -> do
      hurt 7 (other w) Slash
      transmute $
        \_ stackCard ->
          case stackCard of
            StackCard _ Card {card_aspect = Morph} ->
              Just $ Transmutation stackCard (transmuteToCard morphSword stackCard)
            _ ->
              Nothing

morphWand :: Card
morphWand =
  newCard
    Morph
    Wand
    "Hurt for 3 for each card on the wheel,\nthen all MORPH cards on the wheel\nbecome WANDs"
    $ \w -> do
      len <- diasporaLength <$> getStack
      hurt (len * 3) (other w) Slash
      transmute $
        \_ stackCard ->
          case stackCard of
            StackCard _ Card {card_aspect = Morph} ->
              Just $ Transmutation stackCard (transmuteToCard morphWand stackCard)
            _ ->
              Nothing

morphGrail :: Card
morphGrail =
  newCard
    Morph
    Grail
    "Heal for 8, then all MORPH cards on\nthe wheel become GRAILs"
    $ \w -> do
      heal 8 w
      transmute $
        \_ stackCard ->
          case stackCard of
            StackCard _ Card {card_aspect = Morph} ->
              Just $ Transmutation stackCard (transmuteToCard morphGrail stackCard)
            _ ->
              Nothing

morphCoin :: Card
morphCoin =
  newCard
    Morph
    Coin
    "The next card becomes a MORPH card"
    $ \_ -> do
      transmuteHead $
        \stackCard ->
          let targetCard :: Card
              targetCard =
                case card_suit . stackcard_card $ stackCard of
                  Sword ->
                    morphSword
                  Wand ->
                    morphWand
                  Grail ->
                    morphGrail
                  Coin ->
                    morphCoin
                  OtherSuit _ ->
                    strangeGlitch
           in transmuteToCard targetCard stackCard

-- Abyss
abyssSword :: Card
abyssSword =
  newCard
    Abyss
    Sword
    "Discard 5 from their deck"
    $ \w -> do
      let mag = 5
      manyIndexed mag $ \i -> mill (other w) $ TimeModifierOutQuint $ 1 * (Ease.outQuad 0.5 * (fromIntegral i / fromIntegral mag))

abyssWand :: Card
abyssWand =
  newCard
    Abyss
    Wand
    "Discard 3 from their deck\nfor each other card in play"
    $ \w -> do
      len <- diasporaLength <$> getStack
      let mag = 3 * len
      manyIndexed mag $ \i -> mill (other w) $ TimeModifierOutQuint $ 1 * (Ease.outQuad 0.5 * (fromIntegral i / fromIntegral mag))

abyssGrail :: Card
abyssGrail =
  newCard
    Abyss
    Grail
    "Both players discard their hands\nthen draw 5"
    $ \w -> do
      discardHand w (\_ _ -> True)
      discardHand (other w) (\_ _ -> True)
      many 5 $ draw w w (TimeModifierOutQuint 0.4)
      many 5 $ draw (other w) (other w) (TimeModifierOutQuint 0.4)

-- Fever
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

feverGrail :: Card
feverGrail =
  newCard
    Fever
    Grail
    "Healing becomes hurting for all\nother cards on the wheel"
    $ \_ ->
      transmute (\i sc -> if i > 0 then Just (Transmutation sc (cardMap (addStatus StatusBlighted) sc)) else Nothing)

feverCoin :: Card
feverCoin =
  newCard
    Fever
    Coin
    "Change card in next socket to\nSTRANGE DREAM\n(Heal for 13)"
    $ \_ ->
      transmuteHead (transmuteToCard strangeDream)

strangeDream :: Card
strangeDream =
  newCard
    Strange
    (OtherSuit "DREAM")
    "Heal for 13"
    $ \w -> do
      heal 13 w

-- Comet
cometSword :: Card
cometSword =
  newCard
    Comet
    Sword
    "Hurt for 7"
    $ \w ->
      hurt 7 (other w) Slash

cometWand :: Card
cometWand =
  newCard
    Comet
    Wand
    "Hurt for 6 for each other card\nyou own on the wheel"
    $ \w -> do
      stack <- getStack
      let diaspora = Stack.diasporaFromStack stack
      let ownCards = filter (w ==) $ stackcard_owner . snd <$> filter (\(i, _) -> i > 0) diaspora
      let mag = length ownCards
      hurt (mag * 6) (other w) Slash

cometGrail :: Card
cometGrail =
  newCard
    Comet
    Grail
    "Play your own copy of card in\nnext socket into previous\nsocket"
    $ \w -> do
      stack <- getStack
      raw $ do
        let mPrevCard = Stack.get stack 11
        let mNextCard = Stack.get stack 1
        case (mPrevCard, mNextCard) of
          (Nothing, Just nextCard) -> do
            let copiedCard = nextCard {stackcard_owner = w}
            let newStack = Stack.set stack 11 (Just copiedCard)
            Alpha.setStack newStack
          _ ->
            return ()
      Beta.null

cometCoin :: Card
cometCoin =
  newCard
    Comet
    Coin
    "Move card in next socket into\nthe previous socket"
    $ \_ -> moveStack (\i _ -> if i == 1 then Just (-1) else Nothing) (TimeModifierOutQuint 400)

-- Other
strangeEnd :: Card
strangeEnd =
  newCard
    Strange
    (OtherSuit "END")
    "You're out of cards,\nhurt yourself for 10"
    $ \w -> hurt 10 w Slash

strangeGlitch :: Card
strangeGlitch =
  newCard
    Strange
    (OtherSuit "GLITCH")
    "You feel like something\nis missing..."
    $ \_ -> do
      Beta.null
      Beta.null
      Beta.null
      discardStack (\i _ -> i == 0)
      raw $ Alpha.setHold True

swords :: [Card]
swords =
  [ blazeSword,
    heavenSword,
    shroomSword,
    bloodSword,
    mirrorSword,
    dualitySword,
    alchemySword,
    crownSword,
    morphSword,
    tideSword,
    abyssSword,
    feverSword,
    emptySword,
    seerSword,
    cometSword
  ]

wands :: [Card]
wands =
  [ blazeWand,
    heavenWand,
    shroomWand,
    bloodWand,
    mirrorWand,
    dualityWand,
    alchemyWand,
    crownWand,
    morphWand,
    tideWand,
    abyssWand,
    feverWand,
    emptyWand,
    seerWand,
    cometWand
  ]

grails :: [Card]
grails =
  [ blazeGrail,
    heavenGrail,
    shroomGrail,
    bloodGrail,
    mirrorGrail,
    dualityGrail,
    alchemyGrail,
    crownGrail,
    morphGrail,
    tideGrail,
    abyssGrail,
    feverGrail,
    emptyGrail,
    seerGrail,
    cometGrail
  ]

coins :: [Card]
coins =
  [ blazeCoin,
    heavenCoin,
    shroomCoin,
    bloodCoin,
    mirrorCoin,
    dualityCoin,
    alchemyCoin,
    crownCoin,
    morphCoin,
    tideCoin,
    feverCoin,
    emptyCoin,
    seerCoin,
    cometCoin
  ]

others :: [Card]
others =
  [ strangeSpore,
    strangeGold,
    strangeDream,
    strangeEnd,
    strangeGlitch
  ]

allCards :: [Card]
allCards = swords ++ wands ++ grails ++ coins ++ others

cardsByName :: Map Text Card
cardsByName = Map.fromList $ fmap (\card -> (cardName card, card)) allCards

cardsByAspect :: Map Aspect [Card]
cardsByAspect = Map.fromListWith (++) $ fmap (\card -> (card_aspect card, [card])) allCards

getAspectCards :: Aspect -> [Card]
getAspectCards aspect = fromMaybe [] $ Map.lookup aspect cardsByAspect

cardsBySuit :: Map Suit [Card]
cardsBySuit = Map.fromListWith (++) $ fmap (\card -> (card_suit card, [card])) allCards

getSuitCards :: Suit -> [Card]
getSuitCards suit = fromMaybe [] $ Map.lookup suit cardsBySuit
