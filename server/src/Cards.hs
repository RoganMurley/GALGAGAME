module Cards where

import Control.Monad (when)
import Data.Monoid ((<>))
import Safe (headMay)

import Model
import Player (WhichPlayer(..))
import Util (shuffle)


-- Striker
dagger :: Card
dagger =
  Card
    "Dagger"
    "Hurt for 7"
    "striker/dagger.svg"
    "dagger.wav"
    (Just Slash)
    $ hurt 7 PlayerB


fireball :: Card
fireball =
  Card
    "Fireball"
    "Hurt for 5 for each card to the right"
    "striker/fireball.svg"
    "fireball.wav"
    Nothing
    $ do
      len <- length <$> getStack
      hurt (len * 5) PlayerB


offering :: Card
offering =
  Card
    "Offering"
    "Hurt yourself for 7, then draw 2"
    "striker/offering.svg"
    "offering.wav"
    Nothing
    $ do
      hurt 7 PlayerA
      draw PlayerA
      draw PlayerA


confound :: Card
confound =
  Card
    "Confound"
    "Shuffle the order of cards to the right"
    "striker/confound.svg"
    "confound.wav"
    Nothing
    $ do
      gen <- getGen
      modStack $ shuffle gen


-- Breaker
hammer :: Card
hammer =
  Card
    "Hammer"
    "Hurt for 8"
    "breaker/hammer.svg"
    "hammer.wav"
    (Just Slash)
    $ hurt 8 PlayerB


lightning :: Card
lightning =
  Card
    "Lightning"
    "Hurt for 4 for each card to the right"
    "breaker/lightning.svg"
    "lightning.wav"
    Nothing
    $ do
      len <- length <$> getStack
      hurt (len * 4) PlayerB


hubris :: Card
hubris =
  Card
    "Hubris"
    "Remove all cards to the right"
    "breaker/hubris.svg"
    "hubris.wav"
    (Just Obliterate)
    $ setStack []


-- Balancer
katana :: Card
katana =
  Card
    "Katana"
    "Hurt for 9"
    "balancer/katana.svg"
    "axe.mp3"
    (Just Slash)
    $ hurt 9 PlayerB


curse :: Card
curse =
  Card
    "Curse"
    "Hurt weakest player for 15"
    "balancer/curse.svg"
    "frostbite.mp3"
    Nothing
    $ do
      let dmg = 15
      paLife <- getLife PlayerA
      pbLife <- getLife PlayerB
      when (paLife <= pbLife) $ hurt dmg PlayerA
      when (pbLife >= paLife) $ hurt dmg PlayerB


bless :: Card
bless =
  Card
    "Bless"
    "Heal weakest player for 15"
    "balancer/bless.svg"
    "oath.wav"
    Nothing
    $ do
      let mag = 15
      paLife <- getLife PlayerA
      pbLife <- getLife PlayerB
      when (paLife <= pbLife) $ heal mag PlayerA
      when (pbLife >= paLife) $ heal mag PlayerB


balance :: Card
balance =
  Card
    "Balance"
    "Change card to the right's owner to weakest player"
    "balancer/balance.svg"
    "feint.wav"
    Nothing
    $ do
      paLife <- getLife PlayerA
      pbLife <- getLife PlayerB
      when (paLife < pbLife) $
        modStackHead (\(StackCard _ c) -> StackCard PlayerB c)
      when (pbLife > paLife) $
        modStackHead (\(StackCard _ c) -> StackCard PlayerA c)


-- Drinker
scythe :: Card
scythe =
  Card
    "Scythe"
    "Lifesteal for 5"
    "drinker/scythe.svg"
    "bite.wav"
    (Just Slash)
    $ lifesteal 5 PlayerB


bloodsucker :: Card
bloodsucker =
  Card
  "Bloodsucker"
  "Lifesteal for 3 for each card to the right"
  "drinker/bloodsucker.svg"
  "succubus.wav"
  Nothing
  $ do
    len <- length <$> getStack
    lifesteal (len * 3) PlayerB


serpent :: Card
serpent =
  Card
    "Serpent"
    ("Add 2 " <> description badApple <> " to their hand")
    "drinker/serpent.svg"
    "siren.wav"
    Nothing
    $ do
      addToHand PlayerB badApple
      addToHand PlayerB badApple


badApple :: Card
badApple =
  Card
    "Bad Apple"
    "Hurt yourself for 8"
    "drinker/bad-apple.svg"
    "song.wav"
    Nothing
    $ hurt 8 PlayerA


reversal :: Card
reversal =
  Card
    "Reversal"
    "Reverse the order of cards to the right"
    "drinker/reversal.svg"
    "reversal.wav"
    Nothing
    $ modStack reverse


-- Watcher
staff :: Card
staff =
  Card
    "Staff"
    "Hurt for 4, then draw 1"
    "watcher/staff.svg"
    "staff.wav"
    (Just Slash)
    $ do
      hurt 4 PlayerB
      draw PlayerA


surge :: Card
surge =
  Card
    "Surge"
    "Hurt for 6 for each of your cards to the right"
    "watcher/surge.svg"
    "fireball.wav"
    Nothing
    $ do
      len <- length . (filter (owner PlayerA)) <$> getStack
      hurt (len * 6) PlayerB


imitate :: Card
imitate =
  Card
    "Imitate"
    "This card becomes a copy of a random card in your hand"
    "watcher/imitate.svg"
    "feint.wav"
    Nothing
    $ do
      gen <- getGen
      hand <- getHand PlayerA
      mCard <- return . headMay . (filter (/= imitate)) . (shuffle gen) $ hand
      case mCard of
        Just c ->
          modStack ((:) (StackCard PlayerA c))
        Nothing ->
          return ()


prophecy :: Card
prophecy =
  Card
    "Prophecy"
    "Return all cards to the right to hand"
    "watcher/prophecy.svg"
    "precognition.wav"
    Nothing
    $ do
      bounceAll PlayerA
      bounceAll PlayerB


-- Shielder
sword :: Card
sword =
  Card
    "Sword"
    "Hurt for 10"
    "shielder/sword.svg"
    "dagger.wav"
    (Just Slash)
    $ hurt 10 PlayerB


potion :: Card
potion =
  Card
    "Potion"
    "Heal for 10"
    "shielder/potion.svg"
    "potion.wav"
    (Just Heal)
    $ heal 10 PlayerA


reflect :: Card
reflect =
  Card
    "Reflect"
    "All cards to the right change owner"
    "shielder/reflect.svg"
    "reflect.wav"
    Nothing
    $ modStackAll changeOwner


-- Bouncer
boomerang :: Card
boomerang =
  Card
    "Boomerang"
    "Hurt for 3, bounce this card to hand"
    "bouncer/boomerang.svg"
    "boomerang.wav"
    (Just Slash)
    $ do
      hurt 3 PlayerB
      addToHand PlayerA boomerang


overwhelm :: Card
overwhelm =
  Card
    "Overwhelm"
    "Hurt for 3 for each card in your hand"
    "bouncer/overwhelm.svg"
    "superego.wav"
    Nothing
    $ do
      len <- length <$> getHand PlayerA
      hurt (len * 3) PlayerB


echo :: Card
echo =
  Card
    "Echo"
    "When the card to the right activates, it does so twice"
    "bouncer/echo.svg"
    "echo.wav"
    Nothing
    $ modStackHead
      (\(StackCard which (Card name desc pic sfx anim e)) ->
        StackCard which (Card name desc pic sfx anim (e >> e)))


feint :: Card
feint =
  Card
    "Feint"
    "Return all of your cards to the right to hand"
    "bouncer/feint.svg"
    "feint.wav"
    Nothing
    $ bounceAll PlayerA


-- Collector
relicblade :: Card
relicblade =
  Card
    "Relicblade"
    "Hurt for 6"
    "collector/relicblade.svg"
    "dagger.wav"
    (Just Slash)
    $ hurt 6 PlayerB


greed :: Card
greed =
  Card
    "Greed"
    "Hurt for 3 for each card in their hand"
    "collector/greed.svg"
    "envy.wav"
    Nothing
    $ do
      len <- length <$> getHand PlayerB
      hurt (len * 3) PlayerB


alchemy :: Card
alchemy =
  Card
    "Alchemy"
    ("Change card to the right to " <> description gold)
    "collector/alchemy.svg"
    "feint.wav"
    Nothing
    $ modStackHead (\(StackCard w _) -> StackCard w gold)


gold :: Card
gold =
  Card
    "Gold"
    "Draw 2"
    "collector/gold.svg"
    "feint.wav"
    Nothing
    $ do
      draw PlayerA
      draw PlayerA
