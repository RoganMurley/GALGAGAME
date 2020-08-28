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
missile :: Card
missile =
  Card
    "Dagger"
    "Hurt for 7"
    "missile.png"
    Red
    $ \w -> hurt 7 (other w) Slash


fireball :: Card
fireball =
  Card
    "Fireball"
    "Hurt for 5 for each other card on the wheel"
    "fireball.png"
    Red
    $ \w -> do
      len <- length <$> getStack
      hurt (len * 5) (other w) Slash


offering :: Card
offering =
  Card
    "Offering"
    "Pay 4 life to draw 2"
    "offering.png"
    Red
    $ \w -> do
      hurt 4 w Slash
      draw w w
      draw w w


confound :: Card
confound =
  Card
    "Chaos"
    "Shuffle the order of all cards on the wheel"
    "confound.png"
    Red
    $ \_ -> do
      Beta.confound
      Beta.null


-- Breaker
hammer :: Card
hammer =
  Card
    "Hammer"
    "Hurt for 8"
    "hammer.png"
    Blue
    $ \w -> hurt 8 (other w) Slash


lightning :: Card
lightning =
  Card
    "Thunder"
    "Hurt for 4 for each other card on the wheel"
    "lightning.png"
    Blue
    $ \w -> do
      len <- length <$> getStack
      hurt (len * 4) (other w) Slash


feint :: Card
feint =
  Card
    "Moon"
    "Return all of your cards on the wheel to hand"
    "feint.png"
    Blue
    $ \w -> bounce (\(StackCard o _) -> w == o)


hubris :: Card
hubris =
  Card
    "Sun"
    "Discard all cards on the wheel"
    "hubris.png"
    Blue
    $ \_ -> discard (const True)


-- Balancer
katana :: Card
katana =
  Card
    "Blade"
    "Hurt for 9"
    "katana.png"
    White
    $ \w -> hurt 9 (other w) Slash


curse :: Card
curse =
  Card
    "Cruelty"
    "Hurt weakest player for 15"
    "curse.png"
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
    "Charity"
    "Heal weakest player for 15"
    "bless.png"
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
    "Balance"
    "Change next card's owner to weakest player"
    "balance.png"
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
    "Scythe"
    "Lifesteal for 5"
    "scythe.png"
    Green
    $ \w -> lifesteal 5 (other w)


bloodsucker :: Card
bloodsucker =
  Card
    "Feast"
    "Lifesteal for 3 for each other card on the wheel"
    "bloodsucker.png"
    Green
    $ \w -> do
      len <- length <$> getStack
      lifesteal (len * 3) (other w)


serpent :: Card
serpent =
  Card
    "Infect"
    ("Add 2 PARASITE cards to their hand")
    "serpent.png"
    Green
    $ \w -> do
      addToHand (other w) parasite
      addToHand (other w) parasite


parasite :: Card
parasite =
  Card
    "Parasite"
    "Hurt yourself for 4"
    "bad-apple.png"
    Green
    $ \w -> do
      hurt 4 w Bite


reversal :: Card
reversal =
  Card
    "Reversal"
    "Reverse the order of all cards on the wheel"
    "reverse.png"
    Green
    $ const Beta.reverse


-- Watcher
staff :: Card
staff =
  Card
    "Staff"
    "Hurt for 4, then draw 1"
    "staff.png"
    Violet
    $ \w -> do
      hurt 4 (other w) Slash
      draw w w


surge :: Card
surge =
  Card
    "Cascade"
    "Hurt for 8 for each CASCADE in play"
    "surge.png"
    Violet
    $ \w -> do
      stack <- getStack
      let count = length . filter (\(StackCard _ (Card name _ _ _ _)) -> name == "Cascade") $ stack
      hurt ((count + 1) * 8) (other w) Slash


prophecy :: Card
prophecy =
  Card
    "Eye"
    "Return all cards on the wheel to hand"
    "prophecy.png"
    Violet
    $ \_ -> bounce (const True)


-- Shielder
grudge :: Card
grudge =
  Card
    "Grudge"
    "Hurt for 3, add a copy of this card to hand"
    "grudge.png"
    Orange
    $ \w -> do
      hurt 3 (other w) Slash
      addToHand w grudge


overwhelm :: Card
overwhelm =
  Card
    "Envy"
    "Hurt for 3 for each card in your hand"
    "overwhelm.png"
    Orange
    $ \w -> do
      len <- length <$> getHand w
      hurt (len * 3) (other w) Slash


potion :: Card
potion =
  Card
    "Love"
    "Heal for 10"
    "potion.png"
    Orange
    $ heal 10


reflect :: Card
reflect =
  Card
    "Mirror"
    "Change the owner of all cards on the wheel"
    "reflect.png"
    Orange
    $ const Beta.reflect


-- Collector
relicblade :: Card
relicblade =
  Card
    "Sting"
    "Hurt for 6"
    "sting.png"
    Yellow
    $ \w -> hurt 6 (other w) Slash


greed :: Card
greed =
  Card
    "Greed"
    "Hurt for 3 for each card in their hand"
    "greed.png"
    Yellow
    $ \w -> do
      len <- length <$> getHand (other w)
      hurt (len * 3) (other w) Slash


mimic :: Card
mimic =
  Card
    "Mimic"
    "Play a copy of a random card in your hand"
    "waxworks.png"
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
    "Alchemy"
    ("Change next card to GOLD")
    "alchemy.png"
    Yellow
    $ \_ -> transmute gold TransmuteCard


gold :: Card
gold =
  Card
    "Gold"
    "Draw 2"
    "gold.png"
    Yellow
    $ \w -> do
      draw w w
      draw w w


echo :: Card
echo =
  Card
    "Echo"
    "When the next card activates it does so twice"
    "echo.png"
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


-- Daily
subjugate :: Card
subjugate =
  Card
    "Subjugate"
    "Discard next card for each card in your hand"
    "subjugate.png"
    Mystery
    $ \w -> do
      handLen <- length <$> getHand w
      discard $ \(i, _) -> i < handLen


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
    "Discard on the wheel in dark zones"
    "unravel.png"
    Mystery
    $ \_ -> do
      rot <- getRot
      discard $ \(i, _) -> odd (i + rot)


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


basicCards :: [Card]
basicCards =
  [ missile
  , hammer
  , katana
  , scythe
  , staff
  , grudge
  , relicblade
  , ritual
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
  ]


allCards :: [Card]
allCards = basicCards ++ specialCards ++ supportCards ++ controlCards ++ otherCards
