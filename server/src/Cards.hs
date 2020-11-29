module Cards where

import Control.Monad (when)
import CardAnim (Hurt(..))
import Card (Aspect(..), Card(..), Suit(..))
import Player (other)
import Safe (headMay)
import Stack (diasporaFromStack, diasporaLength)
import StackCard (StackCard(..), changeOwner)
import Transmutation (Transmutation(..))
import Util (shuffle)

import qualified DSL.Alpha as Alpha
import qualified DSL.Beta as Beta
import DSL.Beta


-- Blaze
blazeSword :: Card
blazeSword =
  Card Blaze Sword
    "Hurt for 7"
    $ \w -> hurt 7 (other w) Slash


blazeWand :: Card
blazeWand =
  Card Blaze Wand
    "Hurt for 5 for each other card\non the wheel"
    $ \w -> do
      len <- diasporaLength <$> getStack
      hurt (len * 5) (other w) Slash


blazeGrail :: Card
blazeGrail =
  Card Blaze Grail
    "Discard your hand, then draw 2"
    $ \w -> do
      discardHand w (\_ _ -> True)
      draw w w
      draw w w


blazeCoin :: Card
blazeCoin =
  Card Blaze Coin
    "Shuffle the order of all other\ncards on the wheel"
    $ \_ -> do
      confound
      Beta.null


-- Heaven
heavenSword :: Card
heavenSword =
  Card Heaven Sword
    "Hurt for 8"
    $ \w -> hurt 8 (other w) Slash


heavenWand :: Card
heavenWand =
  Card Heaven Wand
    "Hurt for 4 for each other card\non the wheel"
    $ \w -> do
      len <- diasporaLength <$> getStack
      hurt (len * 4) (other w) Slash


heavenGrail :: Card
heavenGrail =
  Card Heaven Grail
    "Return all of your cards\non the wheel to hand"
    $ \w -> bounce (\i (StackCard o _) -> i > 0 && w == o)


heavenCoin :: Card
heavenCoin =
  Card Heaven Coin
    "Discard all cards on the wheel"
    $ \_ -> discardStack (\i _ -> i > 0)


-- Duality
dualitySword :: Card
dualitySword =
  Card Duality Sword
    "Hurt for 9"
    $ \w -> hurt 9 (other w) Slash


dualityWand :: Card
dualityWand =
  Card Duality Wand
    "Hurt weakest player for 15"
    $ \w -> do
      let dmg = 15
      paLife <- getLife w
      pbLife <- getLife (other w)
      when (paLife < pbLife) (hurt dmg w Curse)
      when (paLife > pbLife) (hurt dmg (other w) Curse)
      when (paLife == pbLife) Beta.null


dualityGrail :: Card
dualityGrail =
  Card Duality Grail
    "Heal weakest player for 15"
    $ \w -> do
      let mag = 15
      paLife <- getLife w
      pbLife <- getLife (other w)
      when (paLife < pbLife) (heal mag w)
      when (paLife > pbLife) (heal mag (other w))
      when (paLife == pbLife) Beta.null


dualityCoin :: Card
dualityCoin =
  Card Duality Coin
    "Change next card's owner\nto weakest player"
    $ \w -> do
      paLife <- getLife w
      pbLife <- getLife (other w)
      when (paLife > pbLife) (transmuteHead (\(StackCard _ c) -> StackCard (other w) c))
      when (paLife < pbLife) (transmuteHead (\(StackCard _ c) -> StackCard w c))
      when (paLife == pbLife) Beta.null


-- Shroom
shroomSword :: Card
shroomSword =
  Card Shroom Sword
    "Lifesteal for 5"
    $ \w -> lifesteal 5 (other w)


shroomWand :: Card
shroomWand =
  Card Shroom Wand
    "Lifesteal for 3 for each other card\non the wheel"
    $ \w -> do
      len <- diasporaLength <$> getStack
      lifesteal (len * 3) (other w)


shroomGrail :: Card
shroomGrail =
  Card Shroom Grail
    ("Add 2 STRANGE SPORE cards to\ntheir hand")
    $ \w -> do
      addToHand (other w) strangeSpore
      addToHand (other w) strangeSpore


strangeSpore :: Card
strangeSpore =
  Card Strange (OtherSuit "SPORE")
    "Hurt yourself for 4"
    $ \w -> do
      hurt 4 w Bite


shroomCoin :: Card
shroomCoin =
  Card Shroom Coin
    "Reverse the order of all other\ncards on the wheel"
    $ const reversal


-- Blood
bloodSword :: Card
bloodSword =
  Card Blood Sword
    "Pay 4 life to hurt for 12"
    $ \w -> do
      hurt 4 w Slash
      hurt 12 (other w) Slash


bloodWand :: Card
bloodWand =
  Card Blood Wand
    "Both player's life becomes that\nof the weakest"
    $ \w -> do
      lifePa <- getLife w
      lifePb <- getLife (other w)
      if (lifePa > lifePb) then
        (hurt (lifePa - lifePb) w Slash)
      else
        (hurt (lifePb - lifePa) (other w) Slash)

bloodGrail :: Card
bloodGrail =
  Card Blood Grail
    "Pay 4 life to draw 3"
    $ \w -> do
      hurt 4 w Slash
      draw w w
      draw w w
      draw w w


bloodCoin :: Card
bloodCoin =
  Card Blood Coin
    "Pay half your life to discard\nthe next card"
    $ \w -> do
      l <- getLife w
      hurt (l `quot` 2) w Slash
      discardStack (\i _ -> i == 0)


-- Mirage
mirageSword :: Card
mirageSword =
  Card Mirage Sword
    "Hurt for 4, then draw 1"
    $ \w -> do
      hurt 4 (other w) Slash
      draw w w


mirageWand :: Card
mirageWand =
  Card Mirage Wand
    "Hurt for 8 for each MIRAGE WAND\non the wheel"
    $ \w -> do
      diaspora <- diasporaFromStack <$> getStack
      let isMirageWand = \(Card{ card_aspect, card_suit }) -> card_aspect == Mirage && card_suit == Wand
      let count = length . filter (\(_, StackCard{ stackcard_card }) -> isMirageWand stackcard_card) $ diaspora
      hurt (count * 8) (other w) Slash


mirageGrail :: Card
mirageGrail =
  Card Mirage Grail
    "Become a copy of a random card\nin your hand"
    $ \w -> do
      gen <- getGen
      hand <- getHand w
      let mCopyCard = headMay . (shuffle gen) $ hand
      case mCopyCard of
        Just copyCard -> do
          let stackCard = StackCard{ stackcard_card = copyCard, stackcard_owner = w }
          transmuteActive (\_ -> Just stackCard)
          Beta.null
        Nothing ->
         return ()


mirageCoin :: Card
mirageCoin =
  Card Mirage Coin
    "Return all cards on the wheel to hand"
    $ \_ -> bounce (\i _ -> i > 0)


-- Mirror
mirrorSword :: Card
mirrorSword =
  Card Mirror Sword
    "Hurt for 3, add a copy of this card\nto your hand"
    $ \w -> do
      hurt 3 (other w) Slash
      addToHand w mirrorSword


mirrorWand :: Card
mirrorWand =
  Card Mirror Wand
    "Hurt for 3 for each card in your hand"
    $ \w -> do
      len <- length <$> getHand w
      hurt (len * 3) (other w) Slash

mirrorGrail :: Card
mirrorGrail =
  Card Mirror Grail
    "Card in next socket\nactivates twice"
    $ \_ -> do
      raw $ do
        Alpha.modStackHead $
          \(StackCard which (Card aspect suit desc e)) ->
            StackCard which (Card aspect suit desc (\w -> (e w) >> (e w)))
      Beta.null


mirrorCoin :: Card
mirrorCoin =
  Card Mirror Coin
    "Change the owner of all cards\non the wheel"
    $ \_ ->
      transmute $
        \i stackCard -> if i > 0 then Just $ Transmutation stackCard (changeOwner stackCard) else Nothing


-- Alchemy
alchemySword :: Card
alchemySword =
  Card Alchemy Sword
    "Hurt for 6"
    $ \w -> hurt 6 (other w) Slash


alchemyWand :: Card
alchemyWand =
  Card Alchemy Wand
    "Hurt for 3 for each card\nin their hand"
    $ \w -> do
      len <- length <$> getHand (other w)
      hurt (len * 3) (other w) Slash


alchemyGrail :: Card
alchemyGrail =
  Card Alchemy Grail
    "Heal for 10"
    $ heal 10


alchemyCoin :: Card
alchemyCoin =
  Card Alchemy Coin
    "Change card in next socket\nto STRANGE GOLD"
    $ \_ -> transmuteHead (\(StackCard o _) -> StackCard o strangeGold)


strangeGold :: Card
strangeGold =
  Card Strange (OtherSuit "GOLD")
    "Draw 2"
    $ \w -> do
      draw w w
      draw w w


-- Crown
crownSword :: Card
crownSword =
  Card Crown Sword
    "Hurt for 10"
    $ \w -> hurt 10 (other w) Slash


crownWand :: Card
crownWand =
  Card Crown Wand
    "Discard your hand, then hurt\nfor 5 for each card discarded"
    $ \w -> do
      handSize <- length <$> getHand w
      discardHand w (\_ _ -> True)
      hurt (5 * handSize) (other w) Slash


crownGrail :: Card
crownGrail =
  Card Crown Grail
    "Your opponent is forced to\nplay a random card"
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
  Card Crown Coin
    "Discard next card for each\ncard in your hand"
    $ \w -> do
      handLen <- length <$> getHand w
      discardStack $ \i _ -> i < handLen


-- Morph
morphSword :: Card
morphSword =
  Card Morph Sword
    "Hurt for 7, then all MORPH cards on\nthe wheel become SWORDs"
    $ \w -> do
      hurt 7 (other w) Slash
      transmute $
        \_ stackCard ->
          case stackCard of
            StackCard owner (Card{ card_aspect = Morph }) ->
              Just $ Transmutation stackCard (StackCard owner morphSword)
            _ ->
              Nothing


morphWand :: Card
morphWand =
  Card Morph Wand
    "Hurt for 3 for each card on the wheel,\nthen all MORPH cards on the wheel\nbecome WANDs"
    $ \w -> do
      len <- diasporaLength <$> getStack
      hurt (len * 3) (other w) Slash
      transmute $
        \_ stackCard ->
          case stackCard of
            StackCard owner (Card{ card_aspect = Morph }) ->
              Just $ Transmutation stackCard (StackCard owner morphWand)
            _ ->
              Nothing


morphGrail :: Card
morphGrail =
  Card Morph Grail
    "Heal for 8, then all MORPH cards on\nthe wheel become GRAILs"
    $ \w -> do
      heal 8 w
      transmute $
        \_ stackCard ->
          case stackCard of
            StackCard owner (Card{ card_aspect = Morph }) ->
              Just $ Transmutation stackCard (StackCard owner morphGrail)
            _ ->
              Nothing


morphCoin :: Card
morphCoin =
  Card Morph Coin
    "The next card becomes a MORPH card"
    $ \_ -> do
      transmuteHead $
        \stackCard@(StackCard{ stackcard_owner, stackcard_card}) ->
          case card_suit stackcard_card of
            Sword ->
              StackCard stackcard_owner morphSword
            Wand ->
              StackCard stackcard_owner morphWand
            Grail ->
              StackCard stackcard_owner morphGrail
            Coin ->
              StackCard stackcard_owner morphCoin
            OtherSuit _ ->
              stackCard



-- Other
strangeEnd :: Card
strangeEnd =
  Card Strange (OtherSuit "END")
    "You're out of cards,\nhurt yourself for 10"
    $ \w -> hurt 10 w Slash


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
  , morphSword
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
  , morphWand
  ]


grails :: [Card]
grails =
  [ blazeGrail
  , heavenGrail
  , shroomGrail
  , bloodGrail
  , mirageGrail
  , mirrorGrail
  , dualityGrail
  , alchemyGrail
  , crownGrail
  , morphGrail
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
  , morphCoin
  ]


others :: [Card]
others =
  [ strangeSpore
  , strangeGold
  , strangeEnd
  ]


allCards :: [Card]
allCards = swords ++ wands ++ grails ++ coins ++ others
