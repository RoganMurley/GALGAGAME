module Cards where

import Data.List (delete)
import Data.Monoid ((<>))
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
    (Just Slash)
    $ \p -> hurt 7 (other p)


fireball :: Card
fireball =
  Card
    "Fireball"
    "Hurt for 5 for each card to the right"
    "striker/fireball.svg"
    "fireball.wav"
    Nothing
    $ \p m -> hurt (5 * (length . getStack $ m)) (other p) m


offering :: Card
offering =
  Card
    "Offering"
    "Hurt yourself for 7, then draw 2"
    "striker/offering.svg"
    "offering.wav"
    Nothing
    $ \p -> (times 2 (drawCard p)) . (hurt 7 p)


confound :: Card
confound =
  Card
    "Confound"
    "Shuffle the order of cards to the right"
    "striker/confound.svg"
    "confound.wav"
    Nothing
    $ \_ m -> modStack (shuffle (getGen m)) m


-- Breaker
hammer :: Card
hammer =
  Card
    "Hammer"
    "Hurt for 8"
    "breaker/hammer.svg"
    "hammer.wav"
    (Just Slash)
    $ \p -> hurt 8 (other p)


lightning :: Card
lightning =
  Card
    "Lightning"
    "Hurt for 4 for each card to the right"
    "breaker/lightning.svg"
    "lightning.wav"
    Nothing
    $ \p m -> hurt (4 * (length . getStack $ m)) (other p) m


hubris :: Card
hubris =
  Card
    "Hubris"
    "Remove all cards to the right"
    "breaker/hubris.svg"
    "hubris.wav"
    Nothing
    $ \_ -> setStack []


exile :: Card
exile =
  Card
    "Exile"
    "Remove all other copies of card to the right, wherever they are"
    "breaker/exile.svg"
    "echo.wav"
    Nothing
    $ withStackHead eff
  where
    eff :: StackCard -> CardEff
    eff stackCard@(StackCard _ card) _ =
        (both (\p -> modDeck p $ filter (/= card)))
      . (both (\p -> modHand p $ filter (/= card)))
      . (modStack ((:) stackCard))
      . (modStack (filter (\(StackCard _ c) -> c /= card)))


lifeseed :: Card
lifeseed =
  Card
    "Lifeseed"
    ("Add 2 " <> description lifesprout <> " to your hand")
    "breaker/lifeseed.svg"
    "recharge.wav"
    Nothing
    $ \p -> modHand p (times 2 ((:) lifesprout))


plenty :: Card
plenty =
  Card
    "Plenty"
    "Both draw 1"
    "breaker/plenty.svg"
    "feint.wav"
    Nothing
    $ \_ -> both drawCard


lifesprout :: Card
lifesprout =
  Card
    "Lifesprout"
    "Heal for 9"
    "breaker/lifesprout.svg"
    "recharge.wav"
    Nothing
    $ heal 9


duplicate :: Card
duplicate =
  Card
    "Duplicate"
    "Add a copy of the card to the right to your hand"
    "breaker/duplicate.svg"
    "feint.wav"
    Nothing
    $ \p m ->
      case headMay (getStack m) of
        Just (StackCard _ c) ->
          modHand p ((:) c) m
        Nothing ->
          m


surprise :: Card
surprise =
  Card
    "Surprise"
    "Play a random card from your hand"
    "breaker/surprise.svg"
    "feint.wav"
    Nothing
    eff
  where
    eff :: CardEff
    eff p m =
      case mCard of
        Just c ->
          (modStack ((:) (StackCard p c))) .
            (modHand p (delete c))
              $ m
        Nothing ->
          m
      where
        mCard :: Maybe Card
        mCard = headMay . (shuffle (getGen m)) $ getHand p m


-- Balancer
katana :: Card
katana =
  Card
    "Katana"
    "Hurt for 9"
    "balancer/katana.svg"
    "axe.mp3"
    (Just Slash)
    $ \p -> hurt 9 (other p)


curse :: Card
curse =
  Card
    "Curse"
    "Hurt weakest player for 15"
    "balancer/curse.svg"
    "frostbite.mp3"
    Nothing
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
    "Heal weakest player for 15"
    "balancer/bless.svg"
    "oath.wav"
    Nothing
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


balance :: Card
balance =
  Card
    "Balance"
    "Change card to the right's owner to weakest player"
    "balancer/balance.svg"
    "feint.wav"
    Nothing
    $ \_ m -> modStackHead (eff (getLife PlayerA m, getLife PlayerB m)) m
  where
    eff :: (Life, Life) -> StackCard -> StackCard
    eff (lifePA, lifePB) (StackCard which card) = StackCard weakest card
      where
        weakest :: WhichPlayer
        weakest
          | lifePA < lifePB = PlayerA
          | lifePB < lifePA = PlayerB
          | otherwise       = which


-- Drinker
scythe :: Card
scythe =
  Card
    "Scythe"
    "Lifesteal for 5"
    "drinker/scythe.svg"
    "bite.wav"
    (Just Slash)
    $ \p -> lifesteal 5 (other p)


bloodsucker :: Card
bloodsucker =
  Card
  "Bloodsucker"
  "Lifesteal for 3 for each card to the right"
  "drinker/bloodsucker.svg"
  "succubus.wav"
  Nothing
  $ \p m -> lifesteal (3 * (length . getStack $ m)) (other p) m


serpent :: Card
serpent =
  Card
    "Serpent"
    ("Add 2 " <> description badApple <> " to their hand")
    "drinker/serpent.svg"
    "siren.wav"
    Nothing
    $ \p -> modHand (other p) (times 2 ((:) badApple))


badApple :: Card
badApple =
  Card
    "Bad Apple"
    "Hurt yourself for 8"
    "drinker/bad-apple.svg"
    "song.wav"
    Nothing
    $ hurt 8


reversal :: Card
reversal =
  Card
    "Reversal"
    "Reverse the order of cards to the right"
    "drinker/reversal.svg"
    "reversal.wav"
    Nothing
    $ \_ -> modStack reverse


-- Watcher
staff :: Card
staff =
  Card
    "Staff"
    "Hurt for 4, then draw 1"
    "watcher/staff.svg"
    "staff.wav"
    (Just Slash)
    $ \p -> (drawCard p) . (hurt 4 (other p))


surge :: Card
surge =
  Card
    "Surge"
    "Hurt for 6 for each of your cards to the right"
    "watcher/surge.svg"
    "fireball.wav"
    Nothing
    eff
  where
    eff :: CardEff
    eff p m =
      hurt
        (6 * (length . (filter (\(StackCard o _) -> o == p)) . getStack $ m))
          (other p) m


imitate :: Card
imitate =
  Card
    "Imitate"
    "Create a copy of a random card from your hand to the right"
    "watcher/imitate.svg"
    "feint.wav"
    Nothing
    eff
  where
    eff :: CardEff
    eff p m =
      case mCard of
        Just c ->
          modStack ((:) (StackCard p c)) m
        Nothing ->
          m
      where
        mCard :: Maybe Card
        mCard = headMay . (filter (/= imitate)) . (shuffle (getGen m)) $ getHand p m


mindhack :: Card
mindhack =
  Card
    "Mindhack"
    "Obscure their hand"
    "watcher/mindhack.svg"
    "mindhack.wav"
    Nothing
    $ \p -> modHand (other p) (fmap obs)
  where
    obs :: Card -> Card
    obs card
      | card == (obscured card) = card
      | otherwise = obscured card


obscured :: Card -> Card
obscured c =
  Card
    "???"
    "An obscured card"
    "watcher/obscured.svg"
    "resolve.wav"
    Nothing
    $ \p -> modStack $ (:) (StackCard p c)


prophecy :: Card
prophecy =
  Card
    "Prophecy"
    "Return all cards to the right to hand"
    "watcher/prophecy.svg"
    "precognition.wav"
    Nothing
    $ \_ -> (bounceAll PlayerA) . (bounceAll PlayerB)


-- Shielder
sword :: Card
sword =
  Card
    "Sword"
    "Hurt for 10"
    "shielder/sword.svg"
    "dagger.wav"
    (Just Slash)
    $ \p -> hurt 10 (other p)


backfire :: Card
backfire =
  Card
    "Backfire"
    "Hurt for 5 for each of their cards to the right"
    "shielder/backfire.svg"
    "fireball.wav"
    Nothing
    eff
  where
    eff :: CardEff
    eff p m =
      hurt
        (5 * (length . (filter (\(StackCard o _) -> o == (other p))) . getStack $ m))
          (other p) m


soulburn :: Card
soulburn =
  Card
    "Soulburn"
    "Hurt for 30%"
    "shielder/soulburn.svg"
    "envy.wav"
    Nothing
    eff
  where
    eff :: CardEff
    eff p m =
      let
        dmg :: Life
        dmg = round $ 0.3 * ((fromIntegral $ getLife (other p) m) :: Double)
      in
        hurt (max dmg 1) (other p) m


summon :: Card
summon =
  Card
    "Summon"
    ("Add 3 " <> description demon <> " to your hand")
    "shielder/summon.svg"
    "feint.wav"
    Nothing
    $ \p -> modHand p (times 3 ((:) demon))


demon :: Card
demon =
  Card
    "Demon"
    "Hurt for 5"
    "shielder/demon.svg"
    "envy.wav"
    Nothing
    $ \p -> hurt 5 (other p)


potion :: Card
potion =
  Card
    "Potion"
    "Heal for 10"
    "shielder/potion.svg"
    "potion.wav"
    Nothing
    $ heal 10


reflect :: Card
reflect =
  Card
    "Reflect"
    "All cards to the right change owner"
    "shielder/reflect.svg"
    "reflect.wav"
    Nothing
    $ \_ -> modStackAll changeOwner


-- Bouncer
boomerang :: Card
boomerang =
  Card
    "Boomerang"
    "Hurt for 3, bounce this card to hand"
    "bouncer/boomerang.svg"
    "boomerang.wav"
    (Just Slash)
    $ \p -> (modHand p ((:) boomerang)) . (hurt 3 (other p))


overwhelm :: Card
overwhelm =
  Card
    "Overwhelm"
    "Hurt for 3 for each card in your hand"
    "bouncer/overwhelm.svg"
    "superego.wav"
    Nothing
    $ \p m -> hurt (3 * (length . (getHand p) $ m)) (other p) m


echo :: Card
echo =
  Card
    "Echo"
    "When the card to the right activates, it does so twice"
    "bouncer/echo.svg"
    "echo.wav"
    Nothing
    eff
  where
    eff :: CardEff
    eff _ = modStackHead
      (\(StackCard which (Card name desc pic sfx anim e)) ->
        StackCard which (Card name desc pic sfx anim (\w -> (e w) . (e w))))


return' :: Card
return' =
  Card
    "Return"
    "Card to the right returns to hand after activating"
    "bouncer/return.svg"
    "echo.wav"
    Nothing
    eff
  where
    eff :: CardEff
    eff _ = modStackHead
      (\(StackCard o c@(Card n d i s a e)) ->
        StackCard o (Card n d i s a (\w -> (modHand w ((:) c)) . (e w))))


feint :: Card
feint =
  Card
    "Feint"
    "Return all of your cards to the right to hand"
    "bouncer/feint.svg"
    "feint.wav"
    Nothing
    bounceAll


-- Collector
relicblade :: Card
relicblade =
  Card
    "Relicblade"
    "Hurt for 6"
    "collector/relicblade.svg"
    "dagger.wav"
    (Just Slash)
    $ \p -> hurt 6 (other p)


greed :: Card
greed =
  Card
    "Greed"
    "Hurt for 3 for each card in their hand"
    "collector/greed.svg"
    "envy.wav"
    Nothing
    $ \p m -> hurt (3 * (length . (getHand (other p)) $ m)) (other p) m


hoard :: Card
hoard =
  Card
    "Hoard"
    ("Add " <> description gold <> " to your hand")
    "collector/hoard.svg"
    "feint.wav"
    Nothing
    $ \p -> modHand p ((:) gold)


alchemy :: Card
alchemy =
  Card
    "Alchemy"
    ("Change card to the right to " <> description gold)
    "collector/alchemy.svg"
    "feint.wav"
    Nothing
    $ \_ -> modStackHead (\(StackCard w _) -> StackCard w gold)


gold :: Card
gold =
  Card
    "Gold"
    "Draw 2"
    "collector/gold.svg"
    "feint.wav"
    Nothing
    $ \p -> times 2 (drawCard p)


goldrush :: Card
goldrush =
  Card
    "Goldrush"
    "Both players draw for each card they own to the right"
    "collector/goldrush.svg"
    "feint.wav"
    Nothing
    eff
  where
    eff :: CardEff
    eff _ m = both (\p -> times (ownerLen p) (drawCard p)) m
      where
        ownerLen :: WhichPlayer -> Int
        ownerLen w = length . (filter (owned w)) . getStack $ m


-- Potential future cards
soulheal :: Card
soulheal =
  Card
    "Soulheal"
    "Heal for 50%"
    ""
    "resolve.wav"
    Nothing
    $ \p m -> heal (quot (getLife p m) 2) p m
