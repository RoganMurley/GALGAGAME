module Cards where

import Control.Monad (when)
import Data.Monoid ((<>))
import Player (other)
import Safe (headMay)

import Model
import Util (shuffle)


-- Striker
dagger :: Card
dagger =
  Card
    "Dagger"
    "Hurt for 7"
    "striker/dagger.svg"
    $ \w -> betaSlash 7 (other w)


fireball :: Card
fireball =
  Card
    "Fireball"
    "Hurt for 5 for each card to the right"
    "striker/fireball.svg"
    $ \w -> do
      len <- length <$> betaGetStack
      betaSlash (len * 5) (other w)


offering :: Card
offering =
  Card
    "Offering"
    "Hurt yourself for 7, then draw 2"
    "striker/offering.svg"
    $ \w -> do
      betaSlash 7 w
      betaDraw w
      betaDraw w


confound :: Card
confound =
  Card
    "Confound"
    "Shuffle the order of cards to the right"
    "striker/confound.svg"
    $ \_ -> do
      gen <- betaGetGen
      betaNull
      betaRaw $ modStack $ shuffle gen
      betaNull


-- Breaker
hammer :: Card
hammer =
  Card
    "Hammer"
    "Hurt for 8"
    "breaker/hammer.svg"
    $ \w -> betaSlash 8 (other w)


lightning :: Card
lightning =
  Card
    "Lightning"
    "Hurt for 4 for each card to the right"
    "breaker/lightning.svg"
    $ \w -> do
      len <- length <$> betaGetStack
      betaSlash (len * 4) (other w)


hubris :: Card
hubris =
  Card
    "Hubris"
    "Remove all cards to the right"
    "breaker/hubris.svg"
    $ \_ -> do
      betaNull
      betaRaw $ setStack []
      betaNull


-- Balancer
katana :: Card
katana =
  Card
    "Katana"
    "Hurt for 9"
    "balancer/katana.svg"
    $ \w -> betaSlash 9 (other w)


curse :: Card
curse =
  Card
    "Curse"
    "Hurt weakest player for 15"
    "balancer/curse.svg"
    $ \w -> do
      let dmg = 15
      paLife <- betaGetLife w
      pbLife <- betaGetLife (other w)
      when (paLife <= pbLife) (betaSlash dmg w)
      when (paLife >= pbLife) (betaSlash dmg (other w))


bless :: Card
bless =
  Card
    "Bless"
    "Heal weakest player for 15"
    "balancer/bless.svg"
    $ \w -> do
      let mag = 15
      paLife <- betaGetLife w
      pbLife <- betaGetLife (other w)
      when (paLife <= pbLife) (betaHeal mag w)
      when (paLife >= pbLife) (betaHeal mag (other w))


balance :: Card
balance =
  Card
    "Balance"
    "Change card to the right's owner to weakest player"
    "balancer/balance.svg"
    $ \w -> do
      paLife <- betaGetLife w
      pbLife <- betaGetLife (other w)
      betaNull
      betaRaw $ do
        when (paLife < pbLife) (modStackHead (\(StackCard _ c) -> StackCard w c))
        when (paLife > pbLife) (modStackHead (\(StackCard _ c) -> StackCard (other w) c))
      betaNull


-- Drinker
scythe :: Card
scythe =
  Card
    "Scythe"
    "Lifesteal for 5"
    "drinker/scythe.svg"
    $ \w -> betaLifesteal 5 (other w)


bloodsucker :: Card
bloodsucker =
  Card
    "Bloodsucker"
    "Lifesteal for 3 for each card to the right"
    "drinker/bloodsucker.svg"
    $ \w -> do
      len <- length <$> betaGetStack
      betaLifesteal (len * 3) (other w)


serpent :: Card
serpent =
  Card
    "Serpent"
    ("Add 2 " <> description badApple <> " to their hand")
    "drinker/serpent.svg"
    $ \w -> do
      betaAddToHand (other w) badApple
      betaAddToHand (other w) badApple


badApple :: Card
badApple =
  Card
    "Bad Apple"
    "Hurt yourself for 8"
    "drinker/bad-apple.svg"
    $ \w -> do
      betaSlash 8 w


reversal :: Card
reversal =
  Card
    "Reversal"
    "Reverse the order of cards to the right"
    "drinker/reversal.svg"
    $ \_ -> do
      betaNull
      betaRaw $ modStack reverse
      betaNull


-- Watcher
staff :: Card
staff =
  Card
    "Staff"
    "Hurt for 4, then draw 1"
    "watcher/staff.svg"
    $ \w -> do
      betaSlash 4 (other w)
      betaDraw w


surge :: Card
surge =
  Card
    "Surge"
    "Hurt for 6 for each of your cards to the right"
    "watcher/surge.svg"
    $ \w -> do
      len <- length . (filter (owner w)) <$> betaGetStack
      betaSlash (len * 6) (other w)


imitate :: Card
imitate =
  Card
    "Imitate"
    "This card becomes a copy of a random card in your hand"
    "watcher/imitate.svg"
    $ \w -> do
      betaNull
      betaRaw $ do
        gen <- getGen
        hand <- getHand w
        mCard <- return . headMay . (filter (/= imitate)) . (shuffle gen) $ hand
        case mCard of
          Just c ->
            modStack ((:) (StackCard w c))
          Nothing ->
            return ()
      betaNull


prophecy :: Card
prophecy =
  Card
    "Prophecy"
    "Return all cards to the right to hand"
    "watcher/prophecy.svg"
    $ \w -> do
      betaNull
      betaRaw $ do
        bounceAll w
        bounceAll (other w)
      betaNull


-- Shielder
sword :: Card
sword =
  Card
    "Sword"
    "Hurt for 10"
    "shielder/sword.svg"
    $ \w -> betaSlash 10 (other w)


potion :: Card
potion =
  Card
    "Potion"
    "Heal for 10"
    "shielder/potion.svg"
    $ betaHeal 10


reflect :: Card
reflect =
  Card
    "Reflect"
    "All cards to the right change owner"
    "shielder/reflect.svg"
    $ \_ -> do
      betaNull
      betaRaw $ modStackAll changeOwner
      betaNull


-- Bouncer
boomerang :: Card
boomerang =
  Card
    "Boomerang"
    "Hurt for 3, bounce this card to hand"
    "bouncer/boomerang.svg"
    $ \w -> do
      betaSlash 3 (other w)
      betaAddToHand w boomerang


overwhelm :: Card
overwhelm =
  Card
    "Overwhelm"
    "Hurt for 3 for each card in your hand"
    "bouncer/overwhelm.svg"
    $ \w -> do
      len <- length <$> betaGetHand w
      betaSlash (len * 3) (other w)


echo :: Card
echo =
  Card
    "Echo"
    "When the card to the right activates, it does so twice"
    "bouncer/echo.svg"
    $ \_ -> do
      betaRaw $ do
        modStackHead $
          \(StackCard which (Card name desc pic e)) ->
            StackCard which (Card name desc pic (\w -> (e w) >> (e w)))
      betaNull


feint :: Card
feint =
  Card
    "Feint"
    "Return all of your cards to the right to hand"
    "bouncer/feint.svg"
    $ \w -> do
      betaNull
      betaRaw $ bounceAll w
      betaNull


-- Collector
relicblade :: Card
relicblade =
  Card
    "Relicblade"
    "Hurt for 6"
    "collector/relicblade.svg"
    $ \w -> betaSlash 6 (other w)


greed :: Card
greed =
  Card
    "Greed"
    "Hurt for 3 for each card in their hand"
    "collector/greed.svg"
    $ \w -> do
      len <- length <$> betaGetHand (other w)
      betaSlash (len * 3) (other w)


alchemy :: Card
alchemy =
  Card
    "Alchemy"
    ("Change card to the right to " <> description gold)
    "collector/alchemy.svg"
    $ \_ -> do
      betaNull
      betaRaw $ modStackHead (\(StackCard o _) -> StackCard o gold)
      betaNull


gold :: Card
gold =
  Card
    "Gold"
    "Draw 2"
    "collector/gold.svg"
    $ \w -> do
      betaDraw w
      betaDraw w
