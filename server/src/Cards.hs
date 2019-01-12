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
dagger :: Card
dagger =
  Card
    "Stab"
    "Hurt for 7"
    "striker/dagger.svg"
    $ \w -> slash 7 (other w)


fireball :: Card
fireball =
  Card
    "Fireball"
    "Hurt for 5 for each other card in play"
    "striker/fireball.svg"
    $ \w -> do
      len <- length <$> getStack
      slash (len * 5) (other w)


offering :: Card
offering =
  Card
    "Offering"
    "Pay 4 life to draw 2"
    "striker/offering.svg"
    $ \w -> do
      slash 4 w
      draw w
      draw w


confound :: Card
confound =
  Card
    "Confound"
    "Shuffle the order of all cards in play"
    "striker/confound.svg"
    $ \_ -> do
      gen <- getGen
      raw $ Alpha.modStack $ shuffle gen
      Beta.null


-- Breaker
hammer :: Card
hammer =
  Card
    "Hammer"
    "Hurt for 8"
    "breaker/hammer.svg"
    $ \w -> slash 8 (other w)


lightning :: Card
lightning =
  Card
    "Lightning"
    "Hurt for 4 for each other card in play"
    "breaker/lightning.svg"
    $ \w -> do
      len <- length <$> getStack
      slash (len * 4) (other w)


hubris :: Card
hubris =
  Card
    "Hubris"
    "Discard all cards in play"
    "breaker/hubris.svg"
    $ \_ -> do
      Beta.hubris


-- Balancer
katana :: Card
katana =
  Card
    "Cut"
    "Hurt for 9"
    "balancer/katana.svg"
    $ \w -> slash 9 (other w)


curse :: Card
curse =
  Card
    "Curse"
    "Hurt weakest player for 15"
    "balancer/curse.svg"
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
    "balancer/bless.svg"
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
    "balancer/balance.svg"
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
    "drinker/scythe.svg"
    $ \w -> lifesteal 5 (other w)


bloodsucker :: Card
bloodsucker =
  Card
    "Feast"
    "Lifesteal for 3 for each other card in play"
    "drinker/bloodsucker.svg"
    $ \w -> do
      len <- length <$> getStack
      lifesteal (len * 3) (other w)


serpent :: Card
serpent =
  Card
    "Beguile"
    ("Add 2 " <> description badApple <> " to their hand")
    "drinker/serpent.svg"
    $ \w -> do
      addToHand (other w) badApple
      addToHand (other w) badApple


badApple :: Card
badApple =
  Card
    "Bad Apple"
    "Hurt yourself for 4"
    "drinker/bad-apple.svg"
    $ \w -> do
      bite 4 w


reversal :: Card
reversal =
  Card
    "Reverse"
    "Reverse the order of all cards in play"
    "drinker/reversal.svg"
    $ const Beta.reverse


-- Watcher
staff :: Card
staff =
  Card
    "Smack"
    "Hurt for 4, then draw 1"
    "watcher/staff.svg"
    $ \w -> do
      slash 4 (other w)
      draw w


surge :: Card
surge =
  Card
    "Brainbomb"
    "Hurt for 10 for each 'Brainbomb' in play"
    "watcher/surge.svg"
    $ \w -> do
      stack <- getStack
      let count = length . filter (\(StackCard _ (Card name _ _ _)) -> name == "Brainbomb") $ stack
      slash ((count + 1) * 10) (other w)


mimic :: Card
mimic =
  Card
    "Waxworks"
    "Play a copy of a random card in your hand"
    "honeymaker/waxworks.svg"
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
    "watcher/prophecy.svg"
    $ \_ -> bounce (const True)


-- Shielder
sword :: Card
sword =
  Card
    "Slash"
    "Hurt for 10"
    "shielder/sword.svg"
    $ \w -> slash 10 (other w)


potion :: Card
potion =
  Card
    "Potion"
    "Heal for 10"
    "shielder/potion.svg"
    $ heal 10


reflect :: Card
reflect =
  Card
    "Reflect"
    "All cards in play change owner"
    "shielder/reflect.svg"
    $ const Beta.reflect


-- Bouncer
boomerang :: Card
boomerang =
  Card
    "Boomerang"
    "Hurt for 3, return this card to hand"
    "bouncer/boomerang.svg"
    $ \w -> do
      slash 3 (other w)
      addToHand w boomerang


overwhelm :: Card
overwhelm =
  Card
    "Overwhelm"
    "Hurt for 3 for each card in your hand"
    "bouncer/overwhelm.svg"
    $ \w -> do
      len <- length <$> getHand w
      slash (len * 3) (other w)


echo :: Card
echo =
  Card
    "Echo"
    "When the next card activates it does so twice"
    "bouncer/echo.svg"
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
    "bouncer/feint.svg"
    $ \w -> bounce (\(StackCard o _) -> w == o)


-- Collector
relicblade :: Card
relicblade =
  Card
    "Sting"
    "Hurt for 6"
    "honeymaker/sting.svg"
    $ \w -> slash 6 (other w)


greed :: Card
greed =
  Card
    "Greed"
    "Hurt for 3 for each card in their hand"
    "collector/greed.svg"
    $ \w -> do
      len <- length <$> getHand (other w)
      slash (len * 3) (other w)


alchemy :: Card
alchemy =
  Card
    "Alchemy"
    ("Change next card to " <> description gold)
    "collector/alchemy.svg"
    $ \_ -> transmute gold


gold :: Card
gold =
  Card
    "Honey"
    "Draw 2"
    "honeymaker/honey.svg"
    $ \w -> do
      draw w
      draw w


theEnd :: Card
theEnd =
  Card
    "The End"
    "You're out of cards, hurt yourself for 10"
    "the_end.svg"
    $ slash 10


allCards :: [Card]
allCards =
  [ dagger
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
  , badApple
  , reversal
  , staff
  , surge
  , mimic
  , prophecy
  , sword
  , potion
  , reflect
  , boomerang
  , overwhelm
  , echo
  , feint
  , relicblade
  , greed
  , alchemy
  , gold
  , theEnd
  ]
