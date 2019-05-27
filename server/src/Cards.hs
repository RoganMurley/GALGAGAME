module Cards where

import Control.Monad (when)
import Card (Card(Card), description)
import Data.Monoid ((<>))
import Player (other)
import Safe (headMay)
import StackCard (StackCard(StackCard))
import Util (shuffle)

import qualified DSL.Alpha as Alpha
import qualified DSL.Beta as Beta
import DSL.Beta hiding (hubris, reflect)


-- Striker
missile :: Card
missile =
  Card
    "missile"
    "Hurt for 7"
    "missile.png"
    $ \w -> slash 7 (other w)


fireball :: Card
fireball =
  Card
    "Fireball"
    "Hurt for 5 for each other card in play"
    "fireball.png"
    $ \w -> do
      len <- length <$> getStack
      slash (len * 5) (other w)


offering :: Card
offering =
  Card
    "Offering"
    "Pay 4 life to draw 2"
    "offering.png"
    $ \w -> do
      slash 4 w
      draw w
      draw w


confound :: Card
confound =
  Card
    "Confound"
    "Shuffle the order of all cards in play"
    "confound.png"
    $ \_ -> do
      gen <- getGen
      raw $ Alpha.modStack $ shuffle gen
      Beta.null


-- Breaker
hammer :: Card
hammer =
  Card
    "Shot"
    "Hurt for 8"
    "hammer.png"
    $ \w -> slash 8 (other w)


lightning :: Card
lightning =
  Card
    "Lightning"
    "Hurt for 4 for each other card in play"
    "lightning.png"
    $ \w -> do
      len <- length <$> getStack
      slash (len * 4) (other w)


hubris :: Card
hubris =
  Card
    "Hubris"
    "Discard all cards in play"
    "hubris.png"
    $ \_ -> do
      Beta.hubris


-- Balancer
katana :: Card
katana =
  Card
    "Arrow"
    "Hurt for 9"
    "katana.png"
    $ \w -> slash 9 (other w)


curse :: Card
curse =
  Card
    "Curse"
    "Hurt weakest player for 15"
    "curse.png"
    $ \w -> do
      let dmg = 15
      paLife <- getLife w
      pbLife <- getLife (other w)
      when (paLife <= pbLife) (slash dmg w)
      when (paLife >= pbLife) (slash dmg (other w))


bless :: Card
bless =
  Card
    "Bless"
    "Heal weakest player for 15"
    "bless.png"
    $ \w -> do
      let mag = 15
      paLife <- getLife w
      pbLife <- getLife (other w)
      when (paLife <= pbLife) (heal mag w)
      when (paLife >= pbLife) (heal mag (other w))


balance :: Card
balance =
  Card
    "Balance"
    "Change next card's owner to weakest player"
    "balance.png"
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
    "Harvest"
    "Lifesteal for 5"
    "scythe.png"
    $ \w -> lifesteal 5 (other w)


bloodsucker :: Card
bloodsucker =
  Card
    "Feast"
    "Lifesteal for 3 for each other card in play"
    "bloodsucker.png"
    $ \w -> do
      len <- length <$> getStack
      lifesteal (len * 3) (other w)


serpent :: Card
serpent =
  Card
    "Infect"
    ("Add 2 " <> description parasite <> " to their hand")
    "serpent.png"
    $ \w -> do
      addToHand (other w) parasite
      addToHand (other w) parasite


parasite :: Card
parasite =
  Card
    "Parasite"
    "Hurt yourself for 4"
    "bad-apple.png"
    $ \w -> do
      bite 4 w


reversal :: Card
reversal =
  Card
    "Reversal"
    "Reverse the order of all cards in play"
    "reverse.png"
    $ const Beta.reverse


-- Watcher
staff :: Card
staff =
  Card
    "Dart"
    "Hurt for 4, then draw 1"
    "staff.png"
    $ \w -> do
      slash 4 (other w)
      draw w


surge :: Card
surge =
  Card
    "Brainbomb"
    "Hurt for 10 for each 'Brainbomb' in play"
    "surge.png"
    $ \w -> do
      stack <- getStack
      let count = length . filter (\(StackCard _ (Card name _ _ _)) -> name == "Brainbomb") $ stack
      slash ((count + 1) * 10) (other w)


mimic :: Card
mimic =
  Card
    "Mimic"
    "Play a copy of a random card in your hand"
    "waxworks.png"
    $ \w -> do
      gen <- getGen
      hand <- getHand w
      let mCard = headMay . (filter (/= mimic)) . (shuffle gen) $ hand
      case mCard of
        Just c ->
          fabricate $ StackCard w c
        Nothing ->
          Beta.null


prophecy :: Card
prophecy =
  Card
    "Prophecy"
    "Return all cards in play to hand"
    "prophecy.png"
    $ \_ -> bounce (const True)


-- Shielder
sword :: Card
sword =
  Card
    "Projectile"
    "Hurt for 10"
    "sword.png"
    $ \w -> slash 10 (other w)


potion :: Card
potion =
  Card
    "Potion"
    "Heal for 10"
    "potion.png"
    $ heal 10


reflect :: Card
reflect =
  Card
    "Reflect"
    "Change the owner of all cards in play"
    "reflect.png"
    $ const Beta.reflect


-- Bouncer
grudge :: Card
grudge =
  Card
    "Grudge"
    "Hurt for 3, add a copy of this card to hand"
    "grudge.png"
    $ \w -> do
      slash 3 (other w)
      addToHand w grudge


overwhelm :: Card
overwhelm =
  Card
    "Envy"
    "Hurt for 3 for each card in your hand"
    "overwhelm.png"
    $ \w -> do
      len <- length <$> getHand w
      slash (len * 3) (other w)


echo :: Card
echo =
  Card
    "Echo"
    "When the next card activates it does so twice"
    "echo.png"
    $ \_ -> do
      raw $ do
        Alpha.modStackHead $
          \(StackCard which (Card name desc pic e)) ->
            StackCard which (Card name desc pic (\w -> (e w) >> (e w)))
      Beta.null


feint :: Card
feint =
  Card
    "Feint"
    "Return all of your cards in play to hand"
    "feint.png"
    $ \w -> bounce (\(StackCard o _) -> w == o)


-- Collector
relicblade :: Card
relicblade =
  Card
    "Sting"
    "Hurt for 6"
    "sting.png"
    $ \w -> slash 6 (other w)


greed :: Card
greed =
  Card
    "Greed"
    "Hurt for 3 for each card in their hand"
    "greed.png"
    $ \w -> do
      len <- length <$> getHand (other w)
      slash (len * 3) (other w)


alchemy :: Card
alchemy =
  Card
    "Alchemy"
    ("Change next card to " <> description gold)
    "alchemy.png"
    $ \_ -> transmute gold


gold :: Card
gold =
  Card
    "Gold"
    "Draw 2"
    "gold.png"
    $ \w -> do
      draw w
      draw w


theEnd :: Card
theEnd =
  Card
    "The End"
    "You're out of cards, hurt yourself for 10"
    "end.png"
    $ slash 10


allCards :: [Card]
allCards =
  [ missile
  , fireball
  , offering
  , confound
  , hammer
  , lightning
  , hubris
  , katana
  , curse
  , bless
  , balance
  , scythe
  , bloodsucker
  , serpent
  , parasite
  , reversal
  , staff
  , surge
  , mimic
  , prophecy
  , sword
  , potion
  , reflect
  , grudge
  , overwhelm
  , echo
  , feint
  , relicblade
  , greed
  , alchemy
  , gold
  , theEnd
  ]
