module Cards where

import Control.Monad (when)
import CardAnim (Hurt(..), Transmute(..))
import Card (Card(Card), CardCol(..))
import Player (other)
import Safe (headMay)
import StackCard (StackCard(StackCard))
import Util (shuffle)

import qualified DSL.Alpha as Alpha
import qualified DSL.Beta as Beta
import DSL.Beta hiding (confound, reflect)


-- Blaze
blazeSword :: Card
blazeSword =
  Card
    "BLAZE SWORD"
    "Hurt for 7"
    "cards/blaze/sword.png"
    Red
    $ \w -> hurt 7 (other w) Slash


blazeWand :: Card
blazeWand =
  Card
    "BLAZE WAND"
    "Hurt for 5 for each other card on the wheel"
    "cards/blaze/wand.png"
    Red
    $ \w -> do
      len <- length <$> getStack
      hurt (len * 5) (other w) Slash


blazeCup :: Card
blazeCup =
  Card
    "BLAZE CUP"
    "Discard your hand, then draw 2"
    "cards/blaze/cup.png"
    Red
    $ \w -> do
      discardHand w (const True)
      draw w w
      draw w w


blazeCoin :: Card
blazeCoin =
  Card
    "BLAZE COIN"
    "Shuffle the order of all cards on the wheel"
    "cards/blaze/coin.png"
    Red
    $ \_ -> do
      Beta.confound
      Beta.null


-- Heaven
heavenSword :: Card
heavenSword =
  Card
    "HEAVEN SWORD"
    "Hurt for 8"
    "cards/heaven/sword.png"
    Blue
    $ \w -> hurt 8 (other w) Slash


heavenWand :: Card
heavenWand =
  Card
    "HEAVEN WAND"
    "Hurt for 4 for each other card on the wheel"
    "cards/heaven/wand.png"
    Blue
    $ \w -> do
      len <- length <$> getStack
      hurt (len * 4) (other w) Slash


heavenCup :: Card
heavenCup =
  Card
    "HEAVEN CUP"
    "Return all of your cards on the wheel to hand"
    "cards/heaven/cup.png"
    Blue
    $ \w -> bounce (\(StackCard o _) -> w == o)


heavenCoin :: Card
heavenCoin =
  Card
    "HEAVEN COIN"
    "Discard all cards on the wheel"
    "cards/heaven/coin.png"
    Blue
    $ \_ -> discardStack (const True)


-- Duality
dualitySword :: Card
dualitySword =
  Card
    "DUALITY SWORD"
    "Hurt for 9"
    "cards/duality/sword.png"
    White
    $ \w -> hurt 9 (other w) Slash


dualityWand :: Card
dualityWand =
  Card
    "DUALITY WAND"
    "Hurt weakest player for 15"
    "cards/duality/wand.png"
    White
    $ \w -> do
      let dmg = 15
      paLife <- getLife w
      pbLife <- getLife (other w)
      when (paLife < pbLife) (hurt dmg w Curse)
      when (paLife > pbLife) (hurt dmg (other w) Curse)
      when (paLife == pbLife) Beta.null


dualityCup :: Card
dualityCup =
  Card
    "DUALITY CUP"
    "Heal weakest player for 15"
    "cards/duality/cup.png"
    White
    $ \w -> do
      let mag = 15
      paLife <- getLife w
      pbLife <- getLife (other w)
      when (paLife < pbLife) (heal mag w)
      when (paLife > pbLife) (heal mag (other w))
      when (paLife == pbLife) Beta.null


dualityCoin :: Card
dualityCoin =
  Card
    "DUALITY COIN"
    "Change next card's owner to weakest player"
    "cards/duality/coin.png"
    White
    $ \w -> do
      paLife <- getLife w
      pbLife <- getLife (other w)
      when (paLife > pbLife) (setHeadOwner (other w))
      when (paLife < pbLife) (setHeadOwner w)
      when (paLife == pbLife) Beta.null


-- Shroom
shroomSword :: Card
shroomSword =
  Card
    "SHROOM SWORD"
    "Lifesteal for 5"
    "cards/shroom/sword.png"
    Green
    $ \w -> lifesteal 5 (other w)


shroomWand :: Card
shroomWand =
  Card
    "SHROOM WAND"
    "Lifesteal for 3 for each other card on the wheel"
    "cards/shroom/wand.png"
    Green
    $ \w -> do
      len <- length <$> getStack
      lifesteal (len * 3) (other w)


shroomCup :: Card
shroomCup =
  Card
    "SHROOM CUP"
    ("Add 2 STRANGE SPORE cards to their hand")
    "cards/shroom/cup.png"
    Green
    $ \w -> do
      addToHand (other w) strangeSpore
      addToHand (other w) strangeSpore


strangeSpore :: Card
strangeSpore =
  Card
    "STRANGE SPORE"
    "Hurt yourself for 4"
    "cards/strange/spore.png"
    Green
    $ \w -> do
      hurt 4 w Bite


shroomCoin :: Card
shroomCoin =
  Card
    "SHROOM COIN"
    "Reverse the order of all cards on the wheel"
    "cards/shroom/coin.png"
    Green
    $ const Beta.reverse


-- Blood
bloodSword :: Card
bloodSword =
  Card
    "BLOOD SWORD"
    "Pay 4 life to hurt for 12"
    "cards/blood/sword.png"
    Red
    $ \w -> do
      hurt 4 w Slash
      hurt 12 (other w) Slash


bloodWand :: Card
bloodWand =
  Card
    "BLOOD WAND"
    "Both player's life becomes that of the weakest"
    "cards/blood/wand.png"
    Red
    $ \w -> do
      lifePa <- getLife w
      lifePb <- getLife (other w)
      if (lifePa > lifePb) then
        (hurt (lifePa - lifePb) w Slash)
      else
        (hurt (lifePb - lifePa) (other w) Slash)

bloodCup :: Card
bloodCup =
  Card
    "BLOOD CUP"
    "Pay 4 life to draw 3"
    "cards/blood/cup.png"
    Red
    $ \w -> do
      hurt 4 w Slash
      draw w w
      draw w w
      draw w w


bloodCoin :: Card
bloodCoin =
  Card
    "BLOOD COIN"
    "Pay half your life to discard the next card"
    "cards/blood/coin.png"
    Green
    $ \w -> do
      l <- getLife w
      hurt (l `quot` 2) w Slash
      discardStack (\(i, _) -> i == 0)


-- Mirage
mirageSword :: Card
mirageSword =
  Card
    "MIRAGE SWORD"
    "Hurt for 4, then draw 1"
    "cards/mirage/sword.png"
    Violet
    $ \w -> do
      hurt 4 (other w) Slash
      draw w w


mirageCup :: Card
mirageCup =
  Card
    "MIRAGE CUP"
    "Play a copy of a random card in your hand"
    "cards/mirage/cup.png"
    Violet
    $ \w -> do
      gen <- getGen
      hand <- getHand w
      let mCard = headMay . (shuffle gen) $ hand
      case mCard of
        Just c ->
          fabricate $ StackCard w c
        Nothing ->
          Beta.null


mirageWand :: Card
mirageWand =
  Card
    "MIRAGE WAND"
    "Hurt for 8 for each MIRAGE WAND in play"
    "cards/mirage/wand.png"
    Violet
    $ \w -> do
      stack <- getStack
      let count = length . filter (\(StackCard _ (Card name _ _ _ _)) -> name == "MIRAGE WAND") $ stack
      hurt ((count + 1) * 8) (other w) Slash


mirageCoin :: Card
mirageCoin =
  Card
    "MIRAGE COIN"
    "Return all cards on the wheel to hand"
    "cards/mirage/coin.png"
    Violet
    $ \_ -> bounce (const True)


-- Mirror
mirrorSword :: Card
mirrorSword =
  Card
    "MIRROR SWORD"
    "Hurt for 3, add a copy of this card to your hand"
    "cards/mirror/sword.png"
    Orange
    $ \w -> do
      hurt 3 (other w) Slash
      addToHand w mirrorSword


mirrorWand :: Card
mirrorWand =
  Card
    "MIRROR WAND"
    "Hurt for 3 for each card in your hand"
    "cards/mirror/wand.png"
    Orange
    $ \w -> do
      len <- length <$> getHand w
      hurt (len * 3) (other w) Slash

mirrorCup :: Card
mirrorCup =
  Card
    "MIRROR CUP"
    "The next card activates twice"
    "cards/mirror/cup.png"
    Yellow
    $ \_ -> do
      raw $ do
        Alpha.modStackHead $
          \(StackCard which (Card name desc pic col e)) ->
            StackCard which (Card name desc pic col (\w -> (e w) >> (e w)))
      Beta.null


mirrorCoin :: Card
mirrorCoin =
  Card
    "MIRROR COIN"
    "Change the owner of all cards on the wheel"
    "cards/mirror/coin.png"
    Orange
    $ const Beta.reflect


-- Alchemy
alchemySword :: Card
alchemySword =
  Card
    "ALCHEMY SWORD"
    "Hurt for 6"
    "cards/alchemy/sword.png"
    Yellow
    $ \w -> hurt 6 (other w) Slash


alchemyWand :: Card
alchemyWand =
  Card
    "ALCHEMY WAND"
    "Hurt for 3 for each card in their hand"
    "cards/alchemy/wand.png"
    Yellow
    $ \w -> do
      len <- length <$> getHand (other w)
      hurt (len * 3) (other w) Slash


alchemyCup :: Card
alchemyCup =
  Card
    "ALCHEMY CUP"
    "Heal for 10"
    "cards/alchemy/cup.png"
    Orange
    $ heal 10


alchemyCoin :: Card
alchemyCoin =
  Card
    "ALCHEMY COIN"
    ("Change next card to STRANGE GOLD")
    "cards/alchemy/coin.png"
    Yellow
    $ \_ -> transmute strangeGold TransmuteCard


strangeGold :: Card
strangeGold =
  Card
    "STRANGE GOLD"
    "Draw 2"
    "cards/strange/gold.png"
    Yellow
    $ \w -> do
      draw w w
      draw w w


-- Other
strangeEnd :: Card
strangeEnd =
  Card
    "STRANGE END"
    "You're out of cards, hurt yourself for 10"
    "cards/strange/end.png"
    Mystery
    $ \w -> hurt 10 w Slash


-- Crown
crownSword :: Card
crownSword =
  Card
    "CROWN SWORD"
    "Hurt for 10"
    "cards/crown/sword.png"
    Copper
    $ \w -> hurt 10 (other w) Slash


crownWand :: Card
crownWand =
  Card
    "CROWN WAND"
    "Discard your hand, then hurt for 5 for each card discarded"
    "cards/crown/wand.png"
    Copper
    $ \w -> do
      handSize <- length <$> getHand w
      discardHand w (const True)
      hurt (5 * handSize) (other w) Slash


crownCup :: Card
crownCup =
  Card
    "CROWN CUP"
    "Your opponent is forced to play a random card"
    "cards/crown/cup.png"
    Copper
    $ \w -> do
      gen <- getGen
      hand <- getHand (other w)
      let mCard = headMay . (shuffle gen) $ zip [0..] hand
      case mCard of
        Just (index, c) -> do
          Beta.play (other w) c index
          Beta.windup
        Nothing ->
          Beta.null


crownCoin :: Card
crownCoin =
  Card
    "CROWN COIN"
    "Discard next card for each card in your hand"
    "cards/crown/coin.png"
    Copper
    $ \w -> do
      handLen <- length <$> getHand w
      discardStack $ \(i, _) -> i < handLen


-- Experiments
ritual :: Card
ritual =
  Card
    "Ritual"
    "If zone is dark hurt for 8, or if zone is light heal for 8"
    "ritual.png"
    Mystery
    $ \w -> do
      rot <- getRot
      if even rot then
        hurt 8 (other w) Curse
      else
        heal 8 w


unravel :: Card
unravel =
  Card
    "Unravel"
    "Discard cards on the wheel in dark zones"
    "unravel.png"
    Mystery
    $ \_ -> do
      rot <- getRot
      discardStack $ \(i, _) -> odd (i + rot)


respite :: Card
respite =
  Card
    "Respite"
    "Limbo the next 3 cards"
    "respite.png"
    Mystery
    $ \_ -> do
      limbo $ \(i, _) -> i < 3


voidbeam :: Card
voidbeam =
  Card
    "Voidbeam"
    "Hurt for 10 for each card in limbo"
    "voidbeam.png"
    Mystery
    $ \w -> do
      l <- getLimbo
      let dmg = 10 * length l
      hurt dmg (other w) Slash


feud :: Card
feud =
  Card
    "Feud"
    "Hurt for 2, limbo a copy of this card"
    "feud.png"
    Mystery
    $ \w -> do
      hurt 2 (other w) Slash
      fabricate $ StackCard w feud
      limbo $ \(i, _) -> i == 0


inevitable :: Card
inevitable =
  Card
    "Inevitable"
    "Hurt for 1, limbo a copy of this card with double damage"
    "inevitable.png"
    Mystery
    $ \w -> do
      hurt 1 (other w) Slash
      fabricate $ StackCard (other w) inevitable
      limbo $ \(i, _) -> i == 0


swords :: [Card]
swords =
  [ blazeSword
  , heavenSword
  , shroomSword
  , bloodSword
  , mirageSword
  , mirrorSword
  , dualitySword
  , alchemySword
  , crownSword
  ]


wands :: [Card]
wands =
  [ blazeWand
  , heavenWand
  , shroomWand
  , bloodWand
  , mirageWand
  , mirrorWand
  , dualityWand
  , alchemyWand
  , crownWand
  ]


cups :: [Card]
cups =
  [ blazeCup
  , heavenCup
  , shroomCup
  , bloodCup
  , mirageCup
  , mirrorCup
  , dualityCup
  , alchemyCup
  , crownCup
  ]


coins :: [Card]
coins =
  [ blazeCoin
  , heavenCoin
  , shroomCoin
  , bloodCoin
  , mirageCoin
  , mirrorCoin
  , dualityCoin
  , alchemyCoin
  , crownCoin
  ]


others :: [Card]
others =
  [ strangeSpore
  , strangeGold
  , strangeEnd
  ]


allCards :: [Card]
allCards = swords ++ wands ++ cups ++ coins ++ others
