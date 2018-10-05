module Cards where

import Control.Monad (when)
import Card (Card(Card), description)
import Data.Monoid ((<>))
import Player (other)
import Safe (headMay)
import StackCard (StackCard(StackCard), isOwner)
import Util (shuffle)

import qualified DSL.Alpha as Alpha
import qualified DSL.Beta as Beta
import DSL.Beta hiding (reflect)


-- Striker
dagger :: Card
dagger =
  Card
    "Dagger"
    "Hurt for 7"
    "striker/dagger.svg"
    $ \w -> slash 7 (other w)


fireball :: Card
fireball =
  Card
    "Fireball"
    "Hurt for 5 for each card clockwise"
    "striker/fireball.svg"
    $ \w -> do
      len <- length <$> getStack
      slash (len * 5) (other w)


offering :: Card
offering =
  Card
    "Offering"
    "Pay 7 life to draw 2"
    "striker/offering.svg"
    $ \w -> do
      slash 7 w
      draw w
      draw w


confound :: Card
confound =
  Card
    "Confound"
    "Shuffle the order of cards clockwise"
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
    "Hurt for 4 for each card clockwise"
    "breaker/lightning.svg"
    $ \w -> do
      len <- length <$> getStack
      slash (len * 4) (other w)


hubris :: Card
hubris =
  Card
    "Hubris"
    "Remove all cards clockwise"
    "breaker/hubris.svg"
    $ \_ -> do
      obliterate


-- Balancer
katana :: Card
katana =
  Card
    "Katana"
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
    "Change next card clockwise's owner to weakest player"
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
    "Scythe"
    "Lifesteal for 5"
    "drinker/scythe.svg"
    $ \w -> lifesteal 5 (other w)


bloodsucker :: Card
bloodsucker =
  Card
    "Bloodsucker"
    "Lifesteal for 3 for each card clockwise"
    "drinker/bloodsucker.svg"
    $ \w -> do
      len <- length <$> getStack
      lifesteal (len * 3) (other w)


serpent :: Card
serpent =
  Card
    "Serpent"
    ("Add 2 " <> description badApple <> " to their hand")
    "drinker/serpent.svg"
    $ \w -> do
      addToHand (other w) badApple
      addToHand (other w) badApple


badApple :: Card
badApple =
  Card
    "Bad Apple"
    "Hurt yourself for 8"
    "drinker/bad-apple.svg"
    $ \w -> do
      bite 8 w


reversal :: Card
reversal =
  Card
    "Reversal"
    "Reverse the order of cards clockwise"
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
      slash 4 (other w)
      draw w


surge :: Card
surge =
  Card
    "Surge"
    "Hurt for 6 for each of your cards clockwise"
    "watcher/surge.svg"
    $ \w -> do
      len <- length . (filter (isOwner w)) <$> getStack
      slash (len * 6) (other w)


mimic :: Card
mimic =
  Card
    "Mimic"
    "Play a copy of a random card in your hand"
    "watcher/imitate.svg"
    $ \w -> do
      raw $ do
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
    "Return all cards clockwise to hand"
    "watcher/prophecy.svg"
    $ \w -> do
      raw $ do
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
    "All cards clockwise change owner"
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
    "When the card clockwise activates, it does so twice"
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
    "Return all of your cards clockwise to hand"
    "bouncer/feint.svg"
    $ \w -> do
      raw $ Alpha.bounceAll w
      Beta.null


-- Collector
relicblade :: Card
relicblade =
  Card
    "Relicblade"
    "Hurt for 6"
    "collector/relicblade.svg"
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
    ("Change next card clockwise to " <> description gold)
    "collector/alchemy.svg"
    $ \_ -> transmute gold


gold :: Card
gold =
  Card
    "Gold"
    "Draw 2"
    "collector/gold.svg"
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
