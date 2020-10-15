module Cards where

import Control.Monad (when)
import CardAnim (Hurt(..), Transmute(..))
import Card (Card(Card), CardCol(..))
import Player (other)
import Safe (headMay)
import StackCard (StackCard(StackCard))
import Util (shuffle)

import qualified DSL.Alpha as Alpha
import qualified DSL.Beta as Beta
import DSL.Beta hiding (confound, reflect)


-- Striker
fireSword :: Card
fireSword =
  Card
    "FIRE/SWORD"
    "Hurt for 7"
    "fire-sword.png"
    Red
    $ \w -> hurt 7 (other w) Slash


fireball :: Card
fireball =
  Card
    "FIRE/HEX"
    "Hurt for 5 for each other card on the wheel"
    "fire-wand.png"
    Red
    $ \w -> do
      len <- length <$> getStack
      hurt (len * 5) (other w) Slash


fireCup :: Card
fireCup =
  Card
    "FIRE/GRAIL"
    "Discard your hand, then draw 2"
    "fire-cup.png"
    Red
    $ \w -> do
      discardHand w (const True)
      draw w w
      draw w w


confound :: Card
confound =
  Card
    "FIRE/SEAL"
    "Shuffle the order of all cards on the wheel"
    "fire-circle.png"
    Red
    $ \_ -> do
      Beta.confound
      Beta.null


-- Breaker
hammer :: Card
hammer =
  Card
    "THUNDER/SWORD"
    "Hurt for 8"
    "thunder-sword.png"
    Blue
    $ \w -> hurt 8 (other w) Slash


lightning :: Card
lightning =
  Card
    "THUNDER/HEX"
    "Hurt for 4 for each other card on the wheel"
    "thunder-spell.png"
    Blue
    $ \w -> do
      len <- length <$> getStack
      hurt (len * 4) (other w) Slash


feint :: Card
feint =
  Card
    "THUNDER/GRAIL"
    "Return all of your cards on the wheel to hand"
    "thunder-cup.png"
    Blue
    $ \w -> bounce (\(StackCard o _) -> w == o)


hubris :: Card
hubris =
  Card
    "THUNDER/SEAL"
    "Discard all cards on the wheel"
    "thunder-circle.png"
    Blue
    $ \_ -> discardStack (const True)


-- Balancer
katana :: Card
katana =
  Card
    "SWORD of DUALITY"
    "Hurt for 9"
    "duality-sword.png"
    White
    $ \w -> hurt 9 (other w) Slash


curse :: Card
curse =
  Card
    "WAND of DUALITY"
    "Hurt weakest player for 15"
    "duality-wand.png"
    White
    $ \w -> do
      let dmg = 15
      paLife <- getLife w
      pbLife <- getLife (other w)
      when (paLife < pbLife) (hurt dmg w Curse)
      when (paLife > pbLife) (hurt dmg (other w) Curse)
      when (paLife == pbLife) Beta.null


bless :: Card
bless =
  Card
    "CUP of DUALITY"
    "Heal weakest player for 15"
    "duality-cup.png"
    White
    $ \w -> do
      let mag = 15
      paLife <- getLife w
      pbLife <- getLife (other w)
      when (paLife < pbLife) (heal mag w)
      when (paLife > pbLife) (heal mag (other w))
      when (paLife == pbLife) Beta.null


balance :: Card
balance =
  Card
    "COIN of DUALITY"
    "Change next card's owner to weakest player"
    "duality-coin.png"
    White
    $ \w -> do
      paLife <- getLife w
      pbLife <- getLife (other w)
      when (paLife > pbLife) (setHeadOwner (other w))
      when (paLife < pbLife) (setHeadOwner w)
      when (paLife == pbLife) Beta.null


-- Drinker
scythe :: Card
scythe =
  Card
    "FUNGUS/SWORD"
    "Lifesteal for 5"
    "fungus-sword.png"
    Green
    $ \w -> lifesteal 5 (other w)


bloodsucker :: Card
bloodsucker =
  Card
    "FUNGUS/HEX"
    "Lifesteal for 3 for each other card on the wheel"
    "fungus-spell.png"
    Green
    $ \w -> do
      len <- length <$> getStack
      lifesteal (len * 3) (other w)


serpent :: Card
serpent =
  Card
    "FUNGUS/GRAIL"
    ("Add 2 BAD/FEELING cards to their hand")
    "fungus-cup.png"
    Green
    $ \w -> do
      addToHand (other w) parasite
      addToHand (other w) parasite


parasite :: Card
parasite =
  Card
    "BAD/FEELING"
    "Hurt yourself for 4"
    "bad-feeling.png"
    Green
    $ \w -> do
      hurt 4 w Bite


reversal :: Card
reversal =
  Card
    "FUNGUS/SEAL"
    "Reverse the order of all cards on the wheel"
    "fungus-seal.png"
    Green
    $ const Beta.reverse

-- Blood
bloodSword :: Card
bloodSword =
  Card
    "BLOOD/SWORD"
    "Pay 4 life to hurt for 12"
    "blood-sword.png"
    Red
    $ \w -> do
      hurt 4 w Slash
      hurt 12 (other w) Slash


bloodHex :: Card
bloodHex =
  Card
    "BLOOD/HEX"
    "Both player's life becomes that of the weakest"
    "blood-wand.png"
    Red
    $ \w -> do
      lifePa <- getLife w
      lifePb <- getLife (other w)
      if (lifePa > lifePb) then
        (hurt (lifePa - lifePb) w Slash)
      else
        (hurt (lifePb - lifePa) (other w) Slash)

offering :: Card
offering =
  Card
    "BLOOD/GRAIL"
    "Pay 4 life to draw 3"
    "blood-cup.png"
    Red
    $ \w -> do
      hurt 4 w Slash
      draw w w
      draw w w
      draw w w


sacrifice :: Card
sacrifice =
  Card
    "BLOOD/SEAL"
    "Pay half your life to discard the next card"
    "blood-circle.png"
    Green
    $ \w -> do
      l <- getLife w
      hurt (l `quot` 2) w Slash
      discardStack (\(i, _) -> i == 0)


-- Watcher
staff :: Card
staff =
  Card
    "VISION/SWORD"
    "Hurt for 4, then draw 1"
    "vision-sword.png"
    Violet
    $ \w -> do
      hurt 4 (other w) Slash
      draw w w


surge :: Card
surge =
  Card
    "VISION/HEX"
    "Hurt for 8 for each VISION/HEX in play"
    "vision-wand.png"
    Violet
    $ \w -> do
      stack <- getStack
      let count = length . filter (\(StackCard _ (Card name _ _ _ _)) -> name == "VISION/HEX") $ stack
      hurt ((count + 1) * 8) (other w) Slash


prophecy :: Card
prophecy =
  Card
    "VISION/SEAL"
    "Return all cards on the wheel to hand"
    "vision-circle.png"
    Violet
    $ \_ -> bounce (const True)


-- Shielder
grudge :: Card
grudge =
  Card
    "WATER/SWORD"
    "Hurt for 3, add a WATER/SWORD to your hand"
    "water-sword.png"
    Orange
    $ \w -> do
      hurt 3 (other w) Slash
      addToHand w grudge


overwhelm :: Card
overwhelm =
  Card
    "WATER/HEX"
    "Hurt for 3 for each card in your hand"
    "water-spell.png"
    Orange
    $ \w -> do
      len <- length <$> getHand w
      hurt (len * 3) (other w) Slash


potion :: Card
potion =
  Card
    "WATER/GRAIL"
    "Heal for 10"
    "water-cup.png"
    Orange
    $ heal 10


reflect :: Card
reflect =
  Card
    "WATER/SEAL"
    "Change the owner of all cards on the wheel"
    "water-circle.png"
    Orange
    $ const Beta.reflect


-- Collector
relicblade :: Card
relicblade =
  Card
    "GOLD/SWORD"
    "Hurt for 6"
    "gold-sword.png"
    Yellow
    $ \w -> hurt 6 (other w) Slash


greed :: Card
greed =
  Card
    "GOLD/HEX"
    "Hurt for 3 for each card in their hand"
    "gold-spell.png"
    Yellow
    $ \w -> do
      len <- length <$> getHand (other w)
      hurt (len * 3) (other w) Slash


mimic :: Card
mimic =
  Card
    "VISION/GRAIL"
    "Play a copy of a random card in your hand"
    "vision-cup.png"
    Violet
    $ \w -> do
      gen <- getGen
      hand <- getHand w
      let mCard = headMay . (shuffle gen) $ hand
      case mCard of
        Just c ->
          fabricate $ StackCard w c
        Nothing ->
          Beta.null


alchemy :: Card
alchemy =
  Card
    "GOLD/SEAL"
    ("Change next card to STRANGE/GOLD")
    "gold-circle.png"
    Yellow
    $ \_ -> transmute gold TransmuteCard


gold :: Card
gold =
  Card
    "STRANGE/GOLD"
    "Draw 2"
    "strange-gold.png"
    Yellow
    $ \w -> do
      draw w w
      draw w w


echo :: Card
echo =
  Card
    "GOLD/GRAIL"
    "When the next card activates it does so twice"
    "gold-cup.png"
    Yellow
    $ \_ -> do
      raw $ do
        Alpha.modStackHead $
          \(StackCard which (Card name desc pic col e)) ->
            StackCard which (Card name desc pic col (\w -> (e w) >> (e w)))
      Beta.null


theEnd :: Card
theEnd =
  Card
    "The End"
    "You're out of cards, hurt yourself for 10"
    "end.png"
    Mystery
    $ \w -> hurt 10 w Slash


-- Duelist
lance :: Card
lance =
  Card
    "Lance"
    "Hurt for 10"
    "lance.png"
    Copper
    $ \w -> hurt 10 (other w) Slash


meltdown :: Card
meltdown =
  Card
    "Meltdown"
    "Discard your hand, then hurt for 5 for each card discarded"
    "meltdown.png"
    Copper
    $ \w -> do
      handSize <- length <$> getHand w
      discardHand w (const True)
      hurt (5 * handSize) (other w) Slash


duel :: Card
duel =
  Card
    "Duel"
    "Give each player a PISTOL"
    "duel.png"
    Copper
    $ \w -> do
      addToHand w pistol
      addToHand (other w) pistol


pistol :: Card
pistol =
  Card
    "Pistol"
    "Hurt for 30"
    "pistol.png"
    Copper
    $ \w -> hurt 30 (other w) Slash


taunt :: Card
taunt =
  Card
    "Taunt"
    "Your opponent is forced to play a random card"
    "taunt.png"
    Copper
    $ \w -> do
      gen <- getGen
      hand <- getHand (other w)
      let mCard = headMay . (shuffle gen) $ zip [0..] hand
      case mCard of
        Just (index, c) -> do
          Beta.play (other w) c index
          Beta.windup
        Nothing ->
          Beta.null


-- Daily
subjugate :: Card
subjugate =
  Card
    "Subjugate"
    "Discard next card for each card in your hand"
    "subjugate.png"
    Copper
    $ \w -> do
      handLen <- length <$> getHand w
      discardStack $ \(i, _) -> i < handLen


avarice :: Card
avarice =
  Card
    "Avarice"
    "Hurt for 2 for each card in your and their hand"
    "avarice.png"
    Mystery
    $ \w -> do
      len      <- length <$> getHand w
      lenOther <- length <$> getHand (other w)
      let total = len + lenOther
      hurt (total * 2) (other w) Slash


goldrush :: Card
goldrush =
  Card
    "Goldrush"
    "Both players draw 2"
    "goldrush.png"
    Mystery
    $ \w -> do
      draw w w
      draw (other w) (other w)
      draw w w
      draw (other w) (other w)


telepathy :: Card
telepathy =
  Card
    "Telepathy"
    "Draw 2 from their deck"
    "telepathy.png"
    Mystery
    $ \w -> do
      draw w (other w)
      draw w (other w)


ritual :: Card
ritual =
  Card
    "Ritual"
    "If zone is dark hurt for 8, or if zone is light heal for 8"
    "ritual.png"
    Mystery
    $ \w -> do
      rot <- getRot
      if even rot then
        hurt 8 (other w) Curse
      else
        heal 8 w


unravel :: Card
unravel =
  Card
    "Unravel"
    "Discard cards on the wheel in dark zones"
    "unravel.png"
    Mystery
    $ \_ -> do
      rot <- getRot
      discardStack $ \(i, _) -> odd (i + rot)


respite :: Card
respite =
  Card
    "Respite"
    "Limbo the next 3 cards"
    "respite.png"
    Mystery
    $ \_ -> do
      limbo $ \(i, _) -> i < 3


voidbeam :: Card
voidbeam =
  Card
    "Voidbeam"
    "Hurt for 10 for each card in limbo"
    "voidbeam.png"
    Mystery
    $ \w -> do
      l <- getLimbo
      let dmg = 10 * length l
      hurt dmg (other w) Slash


feud :: Card
feud =
  Card
    "Feud"
    "Hurt for 2, limbo a copy of this card"
    "feud.png"
    Mystery
    $ \w -> do
      hurt 2 (other w) Slash
      fabricate $ StackCard w feud
      limbo $ \(i, _) -> i == 0


inevitable :: Card
inevitable =
  Card
    "Inevitable"
    "Hurt for 1, limbo a copy of this card with double damage"
    "inevitable.png"
    Mystery
    $ \w -> do
      hurt 1 (other w) Slash
      fabricate $ StackCard (other w) inevitable
      limbo $ \(i, _) -> i == 0


sword :: Card
sword =
  Card
    "Projectile"
    "Hurt for 10"
    "sword.png"
    Mystery
    $ \w -> hurt 10 (other w) Slash


-- Basic
basicSword :: Card
basicSword =
  Card
    "SWORD of BEGINNINGS"
    "Hurt for 8"
    "basic-sword.png"
    Mystery
    $ \w -> hurt 8 (other w) Slash

basicWand :: Card
basicWand =
  Card
    "WAND of BEGINNINGS"
    "Hurt for 4 for each other card on the wheel"
    "basic-wand.png"
    Mystery
    $ \w -> do
      len <- length <$> getStack
      hurt (len * 4) (other w) Slash


basicCup :: Card
basicCup =
  Card
    "CUP of BEGINNINGS"
    "Return all of your cards on the wheel to hand"
    "basic-cup.png"
    Mystery
    $ \w -> bounce (\(StackCard o _) -> w == o)


basicCoin :: Card
basicCoin =
  Card
    "COIN of BEGINNINGS"
    "Discard all cards on the wheel"
    "basic-coin.png"
    Mystery
    $ \_ -> discardStack (const True)


basicCards :: [Card]
basicCards =
  [ fireSword
  , hammer
  , katana
  , scythe
  , staff
  , grudge
  , relicblade
  , ritual
  , lance
  , bloodSword
  , basicSword
  ]


specialCards :: [Card]
specialCards =
  [ fireball
  , bloodsucker
  , surge
  , overwhelm
  , curse
  , greed
  , lightning
  , meltdown
  , bloodHex
  , basicWand
  ]


supportCards :: [Card]
supportCards =
  [ offering
  , feint
  , serpent
  , echo
  , potion
  , bless
  , mimic
  , telepathy
  , goldrush
  , taunt
  , basicCup
  ]


controlCards :: [Card]
controlCards =
  [ confound
  , hubris
  , reversal
  , prophecy
  , reflect
  , balance
  , alchemy
  , unravel
  , subjugate
  , basicCoin
  ]


otherCards :: [Card]
otherCards =
  [ parasite
  , gold
  , theEnd
  , respite
  , voidbeam
  , feud
  , inevitable
  , duel
  , pistol
  ]


allCards :: [Card]
allCards = basicCards ++ specialCards ++ supportCards ++ controlCards ++ otherCards
