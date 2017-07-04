module Cards where

import Safe (headMay)

import Model
import Player (WhichPlayer(..), other)
import Util (shuffle, times)


-- Striker
dagger :: Card
dagger =
  Card
    "Dagger"
    "Hurt for 7"
    "striker/dagger.svg"
    "dagger.wav"
    (\p -> hurt 7 (other p))


fireball :: Card
fireball =
  Card
    "Fireball"
    "Hurt for 5 for each card to the right"
    "striker/fireball.svg"
    "fireball.wav"
    (\p m -> hurt (5 * (length . getStack $ m)) (other p) m)


offering :: Card
offering =
  Card
    "Offering"
    "Hurt yourself for 8, then draw two cards"
    "striker/offering.svg"
    "offering.wav"
    (\p -> (drawCard p) . (drawCard p) . (hurt 8 p))


confound :: Card
confound =
  Card
    "Confound"
    "Shuffle the order of cards to the right"
    "striker/confound.svg"
    "confound.wav"
    (\_ m -> modStack (\s -> shuffle s (getGen m)) m)


-- Breaker
hammer :: Card
hammer =
  Card
    "Hammer"
    "Hurt for 8"
    "breaker/hammer.svg"
    "hammer.wav"
    (\p -> hurt 8 (other p))


lightning :: Card
lightning =
  Card
    "Lightning"
    "Hurt for 4 for each card to the right"
    "breaker/lightning.svg"
    "lightning.wav"
    (\p m -> hurt (4 * (length . getStack $ m)) (other p) m)


hubris :: Card
hubris =
  Card
    "Hubris"
    "Negate all cards to the right"
    "breaker/hubris.svg"
    "hubris.wav"
    (\_ -> setStack [])


decree :: Card
decree =
  Card
    "Decree"
    "Negate all other copies of card to the right, wherever they are"
    "breaker/decree.svg"
    "echo.wav"
    eff
  where
    eff :: CardEff
    eff _ m =
      case headMay (getStack m) of
        Nothing ->
          m
        Just stackCard@(StackCard _ card) ->
          (modDeck PlayerA (filter (/= card))) .
            (modDeck PlayerB (filter (/= card))) .
              (modHand PlayerA (filter (/= card))) .
                (modHand PlayerB (filter (/= card))) .
                  (modStack ((:) stackCard)) .
                    (modStack (filter (\(StackCard _ c) -> c /= card)))
                      $ m


-- Seeker
axe :: Card
axe =
  Card
    "Axe"
    "Hurt for 9"
    "seeker/axe.svg"
    "axe.mp3"
    (\p -> hurt 9 (other p))


curse :: Card
curse =
  Card
    "Curse"
    "Hurt the weakest player for 15"
    "seeker/curse.svg"
    "frostbite.mp3"
    eff
  where
    eff :: CardEff
    eff _ m
      | getLife PlayerA m < getLife PlayerB m =
        hurt dmg PlayerA $ m
      | getLife PlayerA m > getLife PlayerB m =
        hurt dmg PlayerB $ m
      | otherwise =
        (hurt dmg PlayerA) . (hurt dmg PlayerB) $ m
    dmg = 15


bless :: Card
bless =
  Card
    "Bless"
    "Heal the weakest player for 15"
    "seeker/bless.svg"
    "oath.wav"
    eff
  where
    eff :: CardEff
    eff _ m
      | getLife PlayerA m < getLife PlayerB m =
        heal mag PlayerA $ m
      | getLife PlayerA m > getLife PlayerB m =
        heal mag PlayerB $ m
      | otherwise =
        (heal mag PlayerA) . (heal mag PlayerB) $ m
    mag = 15


alchemy :: Card
alchemy =
  Card
    "Alchemy"
    "Card to the right's effect becomes: draw 2 cards"
    "seeker/alchemy.svg"
    "feint.wav"
    (\_ -> modStackHead (\(StackCard w _) -> StackCard w gold))
  where
    gold :: Card
    gold =
      Card
        "Gold"
        "Draw 2 cards"
        "gem/gold.svg"
        "feint.wav"
        (\p -> (drawCard p) . (drawCard p))


-- Drinker
scythe :: Card
scythe =
  Card
    "Scythe"
    "Lifesteal for 6"
    "drinker/scythe.svg"
    "bite.wav"
    (\p -> lifesteal 6 (other p))


bloodsucker :: Card
bloodsucker =
  Card
  "Bloodsucker"
  "Lifesteal for 3 for each card to the right"
  "drinker/bloodsucker.svg"
  "succubus.wav"
  (\p m -> lifesteal (3 * (length . getStack $ m)) (other p) m)


serpent :: Card
serpent =
  Card
    "Serpent"
    "They get two Bad Apples that hurt them for 8 each"
    "drinker/serpent.svg"
    "siren.wav"
    (\p -> modHand (other p) (times 2 ((:) badApple)))
  where
    badApple :: Card
    badApple =
      Card
        "Bad Apple"
        "Hurt yourself for 8"
        "drinker/bad-apple.svg"
        "song.wav"
        (hurt 8)


reversal :: Card
reversal =
  Card
    "Reversal"
    "Reverse the order of cards to the right"
    "drinker/reversal.svg"
    "reversal.wav"
    (\_ -> modStack reverse)


-- Trickster
sword :: Card
sword =
  Card
    "Sword"
    "Hurt for 10"
    "trickster/sword.svg"
    "shuriken.wav"
    (\p -> hurt 10 (other p))


overwhelm :: Card
overwhelm =
  Card
    "Overwhelm"
    "Hurt for 3 for each card in your hand"
    "trickster/overwhelm.svg"
    "superego.wav"
    (\p m -> hurt (3 * (length . (getHand p) $ m)) (other p) m)


echo :: Card
echo =
  Card
    "Echo"
    "Card to the right's effect happens twice"
    "trickster/echo.svg"
    "echo.wav"
    eff
  where
    eff :: CardEff
    eff _ = modStackHead
      (\(StackCard which (Card name desc pic sfx e)) ->
        StackCard which (Card name desc pic sfx (\w -> (e w) . (e w))))


feint :: Card
feint =
  Card
    "Feint"
    "Bounce all of your cards to the right to hand"
    "trickster/feint.svg"
    "feint.wav"
    bounceAll


-- Watcher
staff :: Card
staff =
  Card
    "Staff"
    "Hurt for 5"
    "watcher/staff.svg"
    "staff.wav"
    (\p -> hurt 5 (other p))


greed :: Card
greed =
  Card
    "Greed"
    "Hurt for 3 for each card in their hand"
    "watcher/greed.svg"
    "envy.wav"
    (\p m -> hurt (3 * (length . (getHand (other p)) $ m)) (other p) m)


mindhack :: Card
mindhack =
  Card
    "Mindhack"
    "Obscure their hand"
    "watcher/mindhack.svg"
    "mindhack.wav"
    (\p -> modHand (other p) (fmap obs))
  where
    obs :: Card -> Card
    obs card =
      Card
        "???"
        "An obscured card"
        "watcher/obscured.svg"
        "resolve.wav"
        (\p -> modStack ((:) (StackCard p card)))


prophecy :: Card
prophecy =
  Card
    "Prophecy"
    "Bounce all cards to the right to hand"
    "watcher/prophecy.svg"
    "precognition.wav"
    (\_ -> (bounceAll PlayerA) . (bounceAll PlayerB))


-- Ranger
crossbow :: Card
crossbow =
  Card
    "Crossbow"
    "Hurt for 11"
    "ranger/crossbow.svg"
    "crossbow.wav"
    (\p -> hurt 11 (other p))

boomerang :: Card
boomerang =
  Card
    "Boomerang"
    "Hurt for 3, bounce this card to hand"
    "ranger/boomerang.svg"
    "boomerang.wav"
    (\p -> (modHand p ((:) boomerang)) . (hurt 3 (other p)))


potion :: Card
potion =
  Card
    "Potion"
    "Heal for 10"
    "ranger/potion.svg"
    "potion.wav"
    (heal 10)


reflect :: Card
reflect =
  Card
    "Reflect"
    "All cards to the right change owner"
    "ranger/reflect.svg"
    "reflect.wav"
    (\_ -> modStackAll changeOwner)
