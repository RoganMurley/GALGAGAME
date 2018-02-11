module Cards where

import Control.Monad (when)
import Data.Monoid ((<>))
import Model (Card(Card), StackCard(StackCard), changeOwner, description, owner)
import Player (other)
import Safe (headMay)
import Util (shuffle)

import qualified DSL.Alpha as Alpha
import qualified DSL.Beta as Beta


-- Striker
dagger :: Card
dagger =
  Card
    "Dagger"
    "Hurt for 7"
    "striker/dagger.svg"
    $ \w -> Beta.slash 7 (other w)


fireball :: Card
fireball =
  Card
    "Fireball"
    "Hurt for 5 for each card to the right"
    "striker/fireball.svg"
    $ \w -> do
      len <- length <$> Beta.getStack
      Beta.slash (len * 5) (other w)


offering :: Card
offering =
  Card
    "Offering"
    "Hurt yourself for 7, then draw 2"
    "striker/offering.svg"
    $ \w -> do
      Beta.slash 7 w
      Beta.draw w
      Beta.draw w


confound :: Card
confound =
  Card
    "Confound"
    "Shuffle the order of cards to the right"
    "striker/confound.svg"
    $ \_ -> do
      gen <- Beta.getGen
      Beta.null
      Beta.raw $ Alpha.modStack $ shuffle gen
      Beta.null


-- Breaker
hammer :: Card
hammer =
  Card
    "Hammer"
    "Hurt for 8"
    "breaker/hammer.svg"
    $ \w -> Beta.slash 8 (other w)


lightning :: Card
lightning =
  Card
    "Lightning"
    "Hurt for 4 for each card to the right"
    "breaker/lightning.svg"
    $ \w -> do
      len <- length <$> Beta.getStack
      Beta.slash (len * 4) (other w)


hubris :: Card
hubris =
  Card
    "Hubris"
    "Remove all cards to the right"
    "breaker/hubris.svg"
    $ \_ -> do
      Beta.null
      Beta.obliterate


-- Balancer
katana :: Card
katana =
  Card
    "Katana"
    "Hurt for 9"
    "balancer/katana.svg"
    $ \w -> Beta.slash 9 (other w)


curse :: Card
curse =
  Card
    "Curse"
    "Hurt weakest player for 15"
    "balancer/curse.svg"
    $ \w -> do
      let dmg = 15
      paLife <- Beta.getLife w
      pbLife <- Beta.getLife (other w)
      when (paLife <= pbLife) (Beta.slash dmg w)
      when (paLife >= pbLife) (Beta.slash dmg (other w))


bless :: Card
bless =
  Card
    "Bless"
    "Heal weakest player for 15"
    "balancer/bless.svg"
    $ \w -> do
      let mag = 15
      paLife <- Beta.getLife w
      pbLife <- Beta.getLife (other w)
      when (paLife <= pbLife) (Beta.heal mag w)
      when (paLife >= pbLife) (Beta.heal mag (other w))


balance :: Card
balance =
  Card
    "Balance"
    "Change card to the right's owner to weakest player"
    "balancer/balance.svg"
    $ \w -> do
      paLife <- Beta.getLife w
      pbLife <- Beta.getLife (other w)
      Beta.null
      Beta.raw $ do
        when (paLife < pbLife) (Alpha.modStackHead (\(StackCard _ c) -> StackCard w c))
        when (paLife > pbLife) (Alpha.modStackHead (\(StackCard _ c) -> StackCard (other w) c))
      Beta.null


-- Drinker
scythe :: Card
scythe =
  Card
    "Scythe"
    "Lifesteal for 5"
    "drinker/scythe.svg"
    $ \w -> Beta.lifesteal 5 (other w)


bloodsucker :: Card
bloodsucker =
  Card
    "Bloodsucker"
    "Lifesteal for 3 for each card to the right"
    "drinker/bloodsucker.svg"
    $ \w -> do
      len <- length <$> Beta.getStack
      Beta.lifesteal (len * 3) (other w)


serpent :: Card
serpent =
  Card
    "Serpent"
    ("Add 2 " <> description badApple <> " to their hand")
    "drinker/serpent.svg"
    $ \w -> do
      Beta.addToHand (other w) badApple
      Beta.addToHand (other w) badApple


badApple :: Card
badApple =
  Card
    "Bad Apple"
    "Hurt yourself for 8"
    "drinker/bad-apple.svg"
    $ \w -> do
      Beta.slash 8 w


reversal :: Card
reversal =
  Card
    "Reversal"
    "Reverse the order of cards to the right"
    "drinker/reversal.svg"
    $ const Beta.reverse


-- Watcher
staff :: Card
staff =
  Card
    "Staff"
    "Hurt for 4, then draw 1"
    "watcher/staff.svg"
    $ \w -> do
      Beta.slash 4 (other w)
      Beta.draw w


surge :: Card
surge =
  Card
    "Surge"
    "Hurt for 6 for each of your cards to the right"
    "watcher/surge.svg"
    $ \w -> do
      len <- length . (filter (owner w)) <$> Beta.getStack
      Beta.slash (len * 6) (other w)


mimic :: Card
mimic =
  Card
    "Mimic"
    "Play a copy of a random card in your hand"
    "watcher/imitate.svg"
    $ \w -> do
      Beta.null
      Beta.raw $ do
        gen <- Alpha.getGen
        hand <- Alpha.getHand w
        mCard <- return . headMay . (filter (/= mimic)) . (shuffle gen) $ hand
        case mCard of
          Just c ->
            Alpha.modStack ((:) (StackCard w c))
          Nothing ->
            return ()
      Beta.null


prophecy :: Card
prophecy =
  Card
    "Prophecy"
    "Return all cards to the right to hand"
    "watcher/prophecy.svg"
    $ \w -> do
      Beta.null
      Beta.raw $ do
        Alpha.bounceAll w
        Alpha.bounceAll (other w)
      Beta.null


-- Shielder
sword :: Card
sword =
  Card
    "Sword"
    "Hurt for 10"
    "shielder/sword.svg"
    $ \w -> Beta.slash 10 (other w)


potion :: Card
potion =
  Card
    "Potion"
    "Heal for 10"
    "shielder/potion.svg"
    $ Beta.heal 10


reflect :: Card
reflect =
  Card
    "Reflect"
    "All cards to the right change owner"
    "shielder/reflect.svg"
    $ \_ -> do
      Beta.null
      Beta.raw $ Alpha.modStackAll changeOwner
      Beta.null


-- Bouncer
boomerang :: Card
boomerang =
  Card
    "Boomerang"
    "Hurt for 3, return this card to hand"
    "bouncer/boomerang.svg"
    $ \w -> do
      Beta.slash 3 (other w)
      Beta.addToHand w boomerang


overwhelm :: Card
overwhelm =
  Card
    "Overwhelm"
    "Hurt for 3 for each card in your hand"
    "bouncer/overwhelm.svg"
    $ \w -> do
      len <- length <$> Beta.getHand w
      Beta.slash (len * 3) (other w)


echo :: Card
echo =
  Card
    "Echo"
    "When the card to the right activates, it does so twice"
    "bouncer/echo.svg"
    $ \_ -> do
      Beta.raw $ do
        Alpha.modStackHead $
          \(StackCard which (Card name desc pic e)) ->
            StackCard which (Card name desc pic (\w -> (e w) >> (e w)))
      Beta.null


feint :: Card
feint =
  Card
    "Feint"
    "Return all of your cards to the right to hand"
    "bouncer/feint.svg"
    $ \w -> do
      Beta.null
      Beta.raw $ Alpha.bounceAll w
      Beta.null


-- Collector
relicblade :: Card
relicblade =
  Card
    "Relicblade"
    "Hurt for 6"
    "collector/relicblade.svg"
    $ \w -> Beta.slash 6 (other w)


greed :: Card
greed =
  Card
    "Greed"
    "Hurt for 3 for each card in their hand"
    "collector/greed.svg"
    $ \w -> do
      len <- length <$> Beta.getHand (other w)
      Beta.slash (len * 3) (other w)


alchemy :: Card
alchemy =
  Card
    "Alchemy"
    ("Change card to the right to " <> description gold)
    "collector/alchemy.svg"
    $ \_ -> do
      Beta.null
      Beta.raw $ Alpha.modStackHead (\(StackCard o _) -> StackCard o gold)
      Beta.null


gold :: Card
gold =
  Card
    "Gold"
    "Draw 2"
    "collector/gold.svg"
    $ \w -> do
      Beta.draw w
      Beta.draw w

theEnd :: Card
theEnd =
  Card
    "The End"
    "You're out of cards, hurt yourself for 10."
    "the_end.svg"
    $ Beta.slash 10
