module Cards where

import Control.Monad (when)
import Data.Monoid ((<>))
import Player (other)
import Safe (headMay)

import Model
import Util (shuffle)


tempAnimFix :: AlphaProgram () -> BetaProgram ()
tempAnimFix p = betaRaw p >> betaNull


-- Striker
dagger :: Card
dagger =
  Card
    "Dagger"
    "Hurt for 7"
    "striker/dagger.svg"
    "dagger.wav"
    $ \w -> do
      betaSlash 7 (other w)


fireball :: Card
fireball =
  Card
    "Fireball"
    "Hurt for 5 for each card to the right"
    "striker/fireball.svg"
    "fireball.wav"
    $ \w -> tempAnimFix $ do
      len <- length <$> getStack
      hurt (len * 5) (other w)


offering :: Card
offering =
  Card
    "Offering"
    "Hurt yourself for 7, then draw 2"
    "striker/offering.svg"
    "offering.wav"
    $ \w -> tempAnimFix $ do
      hurt 7 w
      draw w
      draw w


confound :: Card
confound =
  Card
    "Confound"
    "Shuffle the order of cards to the right"
    "striker/confound.svg"
    "confound.wav"
    $ \_ -> tempAnimFix $ do
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
    $ \w -> betaSlash 8 (other w)


lightning :: Card
lightning =
  Card
    "Lightning"
    "Hurt for 4 for each card to the right"
    "breaker/lightning.svg"
    "lightning.wav"
    $ \w -> tempAnimFix $ do
      len <- length <$> getStack
      hurt (len * 4) (other w)


hubris :: Card
hubris =
  Card
    "Hubris"
    "Remove all cards to the right"
    "breaker/hubris.svg"
    "hubris.wav"
    $ \_ -> tempAnimFix $ do
      setStack []


-- Balancer
katana :: Card
katana =
  Card
    "Katana"
    "Hurt for 9"
    "balancer/katana.svg"
    "axe.mp3"
    $ \w -> betaSlash 9 (other w)


curse :: Card
curse =
  Card
    "Curse"
    "Hurt weakest player for 15"
    "balancer/curse.svg"
    "frostbite.mp3"
    $ \w -> tempAnimFix $ do
      let dmg = 15
      paLife <- getLife w
      pbLife <- getLife (other w)
      when (paLife <= pbLife) (hurt dmg w)
      when (paLife >= pbLife) (hurt dmg (other w))


bless :: Card
bless =
  Card
    "Bless"
    "Heal weakest player for 15"
    "balancer/bless.svg"
    "oath.wav"
    $ \w -> tempAnimFix $ do
      let mag = 15
      paLife <- getLife w
      pbLife <- getLife (other w)
      when (paLife <= pbLife) (heal mag w)
      when (paLife >= pbLife) (heal mag (other w))


balance :: Card
balance =
  Card
    "Balance"
    "Change card to the right's owner to weakest player"
    "balancer/balance.svg"
    "feint.wav"
    $ \w -> tempAnimFix $ do
      paLife <- getLife w
      pbLife <- getLife (other w)
      when (paLife < pbLife) (modStackHead (\(StackCard _ c) -> StackCard w c))
      when (paLife > pbLife) (modStackHead (\(StackCard _ c) -> StackCard (other w) c))


-- Drinker
scythe :: Card
scythe =
  Card
    "Scythe"
    "Lifesteal for 5"
    "drinker/scythe.svg"
    "bite.wav"
    $ \w -> do
      betaLifesteal 5 (other w)


bloodsucker :: Card
bloodsucker =
  Card
    "Bloodsucker"
    "Lifesteal for 3 for each card to the right"
    "drinker/bloodsucker.svg"
    "succubus.wav"
    $ \w -> tempAnimFix $ do
      len <- length <$> getStack
      lifesteal (len * 3) (other w)


serpent :: Card
serpent =
  Card
    "Serpent"
    ("Add 2 " <> description badApple <> " to their hand")
    "drinker/serpent.svg"
    "siren.wav"
    $ \w -> tempAnimFix $ do
      addToHand (other w) badApple
      addToHand (other w) badApple


badApple :: Card
badApple =
  Card
    "Bad Apple"
    "Hurt yourself for 8"
    "drinker/bad-apple.svg"
    "song.wav"
    $ \w -> tempAnimFix $ do
      hurt 8 w


reversal :: Card
reversal =
  Card
    "Reversal"
    "Reverse the order of cards to the right"
    "drinker/reversal.svg"
    "reversal.wav"
    $ \_ -> tempAnimFix $ do
      modStack reverse


-- Watcher
staff :: Card
staff =
  Card
    "Staff"
    "Hurt for 4, then draw 1"
    "watcher/staff.svg"
    "staff.wav"
    $ \w -> do
      betaSlash 4 (other w)
      tempAnimFix $ draw w


surge :: Card
surge =
  Card
    "Surge"
    "Hurt for 6 for each of your cards to the right"
    "watcher/surge.svg"
    "fireball.wav"
    $ \w -> tempAnimFix $ do
      len <- length . (filter (owner w)) <$> getStack
      hurt (len * 6) (other w)


imitate :: Card
imitate =
  Card
    "Imitate"
    "This card becomes a copy of a random card in your hand"
    "watcher/imitate.svg"
    "feint.wav"
    $ \w -> tempAnimFix $ do
      gen <- getGen
      hand <- getHand w
      mCard <- return . headMay . (filter (/= imitate)) . (shuffle gen) $ hand
      case mCard of
        Just c ->
          modStack ((:) (StackCard w c))
        Nothing ->
          return ()


prophecy :: Card
prophecy =
  Card
    "Prophecy"
    "Return all cards to the right to hand"
    "watcher/prophecy.svg"
    "precognition.wav"
    $ \w -> tempAnimFix $ do
      bounceAll w
      bounceAll (other w)


-- Shielder
sword :: Card
sword =
  Card
    "Sword"
    "Hurt for 10"
    "shielder/sword.svg"
    "dagger.wav"
    $ \w -> betaSlash 10 (other w)


potion :: Card
potion =
  Card
    "Potion"
    "Heal for 10"
    "shielder/potion.svg"
    "potion.wav"
    $ \w -> do
      betaHeal 10 w


reflect :: Card
reflect =
  Card
    "Reflect"
    "All cards to the right change owner"
    "shielder/reflect.svg"
    "reflect.wav"
    $ \_ -> tempAnimFix $ do
      modStackAll changeOwner


-- Bouncer
boomerang :: Card
boomerang =
  Card
    "Boomerang"
    "Hurt for 3, bounce this card to hand"
    "bouncer/boomerang.svg"
    "boomerang.wav"
    $ \w -> tempAnimFix $ do
      hurt 3 (other w)
      addToHand w boomerang


overwhelm :: Card
overwhelm =
  Card
    "Overwhelm"
    "Hurt for 3 for each card in your hand"
    "bouncer/overwhelm.svg"
    "superego.wav"
    $ \w -> tempAnimFix $ do
      len <- length <$> getHand w
      hurt (len * 3) (other w)


echo :: Card
echo =
  Card
    "Echo"
    "When the card to the right activates, it does so twice"
    "bouncer/echo.svg"
    "echo.wav"
    $ \_ -> tempAnimFix $ do
      modStackHead $
        \(StackCard which (Card name desc pic sfx e)) ->
          StackCard which (Card name desc pic sfx (\w -> (e w) >> (e w)))


feint :: Card
feint =
  Card
    "Feint"
    "Return all of your cards to the right to hand"
    "bouncer/feint.svg"
    "feint.wav"
    $ \w -> tempAnimFix $ bounceAll w


-- Collector
relicblade :: Card
relicblade =
  Card
    "Relicblade"
    "Hurt for 6"
    "collector/relicblade.svg"
    "dagger.wav"
    $ \w -> betaSlash 6 (other w)


greed :: Card
greed =
  Card
    "Greed"
    "Hurt for 3 for each card in their hand"
    "collector/greed.svg"
    "envy.wav"
    $ \w -> tempAnimFix $ do
      len <- length <$> getHand (other w)
      hurt (len * 3) (other w)


alchemy :: Card
alchemy =
  Card
    "Alchemy"
    ("Change card to the right to " <> description gold)
    "collector/alchemy.svg"
    "feint.wav"
    $ \_ -> tempAnimFix $ do
      modStackHead (\(StackCard o _) -> StackCard o gold)


gold :: Card
gold =
  Card
    "Gold"
    "Draw 2"
    "collector/gold.svg"
    "feint.wav"
    $ \w -> tempAnimFix $ do
      draw w
      draw w
