module Cards where

import Model
import Player (WhichPlayer(..), other)
import Util (shuffle, times)


-- Fire
dagger :: Card
dagger =
  Card
    "Dagger"
    "Hurt for 11"
    "flame/dagger.svg"
    "dagger.wav"
    (\p -> hurt 11 (other p))


fireball :: Card
fireball =
  Card
    "Fireball"
    "Hurt for 5 for each card to the right"
    "flame/fireball.svg"
    "fireball.wav"
    (\p m -> hurt (5 * (length . getStack $ m)) (other p) m)


offering :: Card
offering =
  Card
    "Offering"
    "Hurt yourself for 8, then draw two cards"
    "flame/offering.svg"
    "offering.wav"
    (\p -> (drawCard p) . (drawCard p) . (hurt 8 p))


confound :: Card
confound =
  Card
    "Confound"
    "Shuffle the order of cards to the right"
    "flame/confound.svg"
    "confound.wav"
    (\_ m -> modStack (\s -> shuffle s (getGen m)) m)


-- Thunder
hammer :: Card
hammer =
  Card
    "Hammer"
    "Hurt for 10"
    "thunder/hammer.svg"
    "hammer.wav"
    (\p -> hurt 10 (other p))


lightning :: Card
lightning =
  Card
    "Lightning"
    "Hurt for 4 for each card to the right"
    "thunder/lightning.svg"
    "lightning.wav"
    (\p m -> hurt (4 * (length . getStack $ m)) (other p) m)


hubris :: Card
hubris =
  Card
    "Hubris"
    "Negate all cards to the right"
    "thunder/hubris.svg"
    "hubris.wav"
    (\_ -> setStack [])


echo :: Card
echo =
  Card
    "Echo"
    "The next card to the right's effect happens twice"
    "thunder/echo.svg"
    "echo.wav"
    eff
  where
    eff :: CardEff
    eff _ = modStackHead
      (\(StackCard which (Card name desc pic sfx e)) ->
        StackCard which (Card name desc pic sfx (\w -> (e w) . (e w))))


-- Seek
axe :: Card
axe =
  Card
    "Axe"
    "Hurt for 9"
    "seek/axe.svg"
    "axe.mp3"
    (\p -> hurt 9 (other p))


curse :: Card
curse =
  Card
    "Curse"
    "Hurt the weakest player for 15"
    "seek/curse.svg"
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
    "seek/bless.svg"
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
    "The next card to the right's effect becomes: draw 2 cards"
    "seek/alchemy.svg"
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


-- Feast
scythe :: Card
scythe =
  Card
    "Scythe"
    "Lifesteal for 8"
    "feast/scythe.svg"
    "bite.wav"
    (\p -> lifesteal 8 (other p))


bloodsucker :: Card
bloodsucker =
  Card
  "Bloodsucker"
  "Lifesteal for 4 for each card to the right"
  "feast/bloodsucker.svg"
  "succubus.wav"
  (\p m -> lifesteal (4 * (length . getStack $ m)) (other p) m)


theBook :: Card
theBook =
  Card
    "The Book"
    "Your opponent gets two cards that hurt them for 8 each"
    "feast/the-book.svg"
    "siren.wav"
    (\p -> modHand (other p) (times 2 ((:) thoughts)))
  where
    thoughts :: Card
    thoughts =
      Card
        "Thoughts"
        "Hurt yourself for 8"
        "feast/thoughts.svg"
        "song.wav"
        (hurt 8)


reversal :: Card
reversal =
  Card
    "Reversal"
    "Reverse the order of cards to the right"
    "feast/reversal.svg"
    "reversal.wav"
    (\_ -> modStack reverse)


-- Trick
shuriken :: Card
shuriken =
  Card
    "Staff"
    "Hurt for 7"
    "trick/shuriken.svg"
    "shuriken.wav"
    (\p -> hurt 7 (other p))


superego :: Card
superego =
  Card
    "Superego"
    "Hurt for 3 for each card in your hand"
    "trick/superego.svg"
    "superego.wav"
    (\p m -> hurt (3 * (length . (getHand p) $ m)) (other p) m)


mindgate :: Card
mindgate =
  Card
    "Mindgate"
    "Your hand becomes the same as your opponent's"
    "trick/mindgate.svg"
    "mindgate.wav"
    (\p m -> setHand p (getHand (other p) m) m)


feint :: Card
feint =
  Card
    "Feint"
    "Bounce all of your cards to the right to hand"
    "trick/feint.svg"
    "feint.wav"
    bounceAll


-- Future
staff :: Card
staff =
  Card
    "Staff"
    "Hurt for 6"
    "future/staff.svg"
    "staff.wav"
    (\p -> hurt 6 (other p))


greed :: Card
greed =
  Card
    "Greed"
    "Hurt for 3 for each card in your opponent's hand"
    "future/greed.svg"
    "envy.wav"
    (\p m -> hurt (3 * (length . (getHand (other p)) $ m)) (other p) m)


mindhack :: Card
mindhack =
  Card
    "Mindhack"
    "Obscure your opponent's hand"
    "future/mindhack.svg"
    "mindhack.wav"
    (\p -> modHand (other p) (fmap obs))
  where
    obs :: Card -> Card
    obs card =
      Card
        "???"
        "An obscured card"
        "future/obscured.svg"
        "resolve.wav"
        (\p -> modStack ((:) (StackCard p card)))


prophecy :: Card
prophecy =
  Card
    "Prophecy"
    "Bounce all cards to the right to hand"
    "future/prophecy.svg"
    "precognition.wav"
    (\_ -> (bounceAll PlayerA) . (bounceAll PlayerB))


-- Shield
crossbow :: Card
crossbow =
  Card
    "Crossbow"
    "Hurt for 5"
    "shield/crossbow.svg"
    "crossbow.wav"
    (\p -> hurt 5 (other p))

boomerang :: Card
boomerang =
  Card
    "Boomerang"
    "Hurt for 3, bounce this card to hand"
    "shield/boomerang.svg"
    "boomerang.wav"
    (\p -> (modHand p ((:) boomerang)) . (hurt 3 (other p)))


potion :: Card
potion =
  Card
    "Potion"
    "Heal for 10"
    "shield/potion.svg"
    "potion.wav"
    (heal 10)


reflect :: Card
reflect =
  Card
    "Reflect"
    "All cards to the right change owner"
    "shield/reflect.svg"
    "reflect.wav"
    (\_ -> modStackAll changeOwner)
