{-# LANGUAGE TemplateHaskell, FlexibleContexts, FlexibleInstances #-}
module Model where

import Prelude hiding (log)

import Control.Monad (when)
import Control.Monad.Free (Free(..), MonadFree, foldFree, liftF)
import Control.Monad.Free.TH (makeFree)
import Data.Aeson (ToJSON(..), (.=), object)
import Data.Functor.Sum (Sum(..))
import Data.List (partition)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Text (Text, toUpper)
import Safe (headMay, tailSafe)
import Text.Printf (printf)

import Mirror (Mirror(..))
import Player (WhichPlayer(..), other)
import Util (Gen)


data Card = Card
  { card_name :: Text
  , card_desc :: Text
  , card_img  :: Text
  , card_eff  :: WhichPlayer -> BetaProgram ()
  }


instance Eq Card where
  (Card n1 d1 i1 _) == (Card n2 d2 i2 _) =
    n1 == n2 && d1 == d2 && i1 == i2


instance Show Card where
  show = cs . card_name


instance ToJSON Card where
  toJSON (Card name desc imageURL _) =
    object
      [
        "name"     .= name
      , "desc"     .= desc
      , "imageURL" .= imageURL
      ]


instance ToJSON CardAnim where
  toJSON (Slash w d) =
    object
      [
       "player" .= w
      , "anim"  .= ("slash" :: Text, d)
      ]
  toJSON (Heal w) =
    object
      [
       "player" .= w
      , "anim"  .= ("heal" :: Text)
      ]
  toJSON (Draw w) =
    object
      [
       "player" .= w
      , "anim"  .= ("draw" :: Text)
      ]
  toJSON Obliterate =
    object
      [
       "player" .= PlayerA
      , "anim"  .= ("obliterate" :: Text)
      ]


instance Mirror CardAnim where
  mirror (Slash w d) = Slash (other w) d
  mirror (Heal w)    = Heal  (other w)
  mirror (Draw w)    = Draw  (other w)
  mirror Obliterate  = Obliterate


data CardAnim =
    Slash WhichPlayer Int
  | Heal WhichPlayer
  | Draw WhichPlayer
  | Obliterate
  deriving (Show, Eq)


data Model = Model
  { model_turn   :: Turn
  , model_stack  :: Stack
  , model_pa     :: PlayerModel
  , model_pb     :: PlayerModel
  , model_passes :: Passes
  , model_gen    :: Gen
  }
  deriving (Eq, Show)


data PlayerModel = PlayerModel
  { pmodel_hand :: Hand
  , pmodel_deck :: Deck
  , pmodel_life :: Life
  }
  deriving (Eq, Show)

type Hand = [Card]
type Deck = [Card]
type Stack = [StackCard]

data StackCard = StackCard
  { stackcard_owner :: WhichPlayer
  , stackcard_card  :: Card
  }
  deriving (Eq, Show)

type Life = Int

type Turn = WhichPlayer

data Passes = NoPass | OnePass
  deriving (Eq, Show)


instance ToJSON Model where
  toJSON Model{ model_turn, model_stack, model_pa, model_pb } =
    object
      [
        "turn"   .= model_turn
      , "stack"  .= model_stack
      , "handPA" .= pmodel_hand model_pa
      , "handPB" .= length (pmodel_hand model_pb)
      , "lifePA" .= pmodel_life model_pa
      , "lifePB" .= pmodel_life model_pb
      ]

instance ToJSON StackCard where
  toJSON StackCard{ stackcard_owner, stackcard_card } =
    object [
      "owner" .= stackcard_owner
    , "card"  .= stackcard_card
    ]

instance Mirror Model where
  mirror (Model turn stack pa pb passes gen) =
    Model (other turn) (mirror <$> stack) pb pa passes gen


instance Mirror StackCard where
  mirror (StackCard p c) = StackCard (other p) c


-- DSLs
toLeft :: (Functor f, Functor g) => Free f a -> Free (Sum f g) a
toLeft (Free f) = Free $ toLeft <$> InL f
toLeft (Pure x) = Pure x


toRight :: (Functor f, Functor g) => Free g a -> Free (Sum f g) a
toRight (Free f) = Free $ toRight <$> InR f
toRight (Pure x) = Pure x


data Alpha n =
    GetGen (Gen -> n)
  | GetDeck WhichPlayer (Deck -> n)
  | GetHand WhichPlayer (Hand-> n)
  | GetLife WhichPlayer (Life -> n)
  | GetPasses (Passes -> n)
  | GetStack (Stack -> n)
  | GetTurn (Turn -> n)
  | SetGen Gen n
  | SetDeck WhichPlayer Deck n
  | SetHand WhichPlayer Hand n
  | SetLife WhichPlayer Life n
  | SetPasses Passes n
  | SetStack Stack n
  | SetTurn Turn n
  deriving (Functor)


data Beta n
  = BetaRaw (AlphaProgram ()) n
  | BetaSlash Life WhichPlayer n
  | BetaHeal Life WhichPlayer n
  | BetaDraw WhichPlayer n
  | BetaAddToHand WhichPlayer Card n
  | BetaGetDeck WhichPlayer (Deck -> n)
  | BetaGetHand WhichPlayer (Hand -> n)
  | BetaGetLife WhichPlayer (Life -> n)
  | BetaGetGen (Gen -> n)
  | BetaGetStack (Stack -> n)
  | BetaNull n
  deriving (Functor)


type AlphaProgram a = Free Alpha a
type BetaProgram a = Free Beta a


makeFree ''Alpha
makeFree ''Beta


-- Beta actions
betaLifesteal :: Life -> WhichPlayer -> BetaProgram ()
betaLifesteal dmg w = do
  betaSlash dmg w
  betaHeal dmg (other w)


-- Constants
maxHandLength :: Int
maxHandLength = 6


maxLife :: Life
maxLife = 50


-- Helper functions
changeOwner :: StackCard -> StackCard
changeOwner = mirror


owner :: WhichPlayer -> StackCard -> Bool
owner w (StackCard o _) = w == o


-- PLAYER MODEL
getPmodel :: WhichPlayer -> (Model -> PlayerModel)
getPmodel PlayerA = model_pa
getPmodel PlayerB = model_pb


setPmodel :: PlayerModel -> WhichPlayer -> Model -> Model
setPmodel pmodel PlayerA model = model { model_pa = pmodel }
setPmodel pmodel PlayerB model = model { model_pb = pmodel }


modPmodel :: (PlayerModel -> PlayerModel) -> WhichPlayer -> Model -> Model
modPmodel f p m = setPmodel (f (getPmodel p m)) p m


owned :: WhichPlayer -> StackCard -> Bool
owned w (StackCard o _) = w == o


description :: Card -> Text
description Card{ card_name, card_desc } =
  "(" <> toUpper card_name <> ": " <> card_desc <> ")"


-- Actions
modifier :: (WhichPlayer -> AlphaProgram a) -> (WhichPlayer -> a -> AlphaProgram ()) -> WhichPlayer -> (a -> a) -> AlphaProgram ()
modifier getter setter w f = do
  x <- getter w
  setter w (f x)


modLife :: WhichPlayer -> (Life -> Life) -> AlphaProgram ()
modLife = modifier getLife setLife

modHand :: WhichPlayer -> (Hand -> Hand) -> AlphaProgram ()
modHand = modifier getHand setHand


modDeck :: WhichPlayer -> (Deck -> Deck) -> AlphaProgram ()
modDeck = modifier getDeck setDeck


modStack :: (Stack -> Stack) -> AlphaProgram ()
modStack f = getStack >>= (setStack . f)


modStackAll :: (StackCard -> StackCard) -> AlphaProgram ()
modStackAll f = modStack $ fmap f


modTurn :: (Turn -> Turn) -> AlphaProgram ()
modTurn f = getTurn >>= (setTurn . f)


modPasses :: (Passes -> Passes) -> AlphaProgram ()
modPasses f = getPasses >>= (setPasses . f)


modStackHead :: (StackCard -> StackCard) -> AlphaProgram ()
modStackHead f = do
  s <- getStack
  case headMay s of
    Just c ->
      setStack $ f c : (tailSafe s)
    Nothing ->
      return ()


hurt :: Life -> WhichPlayer -> AlphaProgram ()
hurt dmg w =
  modLife w (-dmg+)


heal :: Life -> WhichPlayer -> AlphaProgram ()
heal mag w =
  modLife w (+mag)


lifesteal :: Life -> WhichPlayer -> AlphaProgram ()
lifesteal dmg w = do
  hurt dmg w
  heal dmg (other w)


draw :: WhichPlayer -> AlphaProgram ()
draw w =
  do
    deck <- getDeck w
    case headMay deck of
      Just card -> do
        modDeck w tailSafe
        addToHand w card
      Nothing ->
        addToHand w theEnd
  where
    theEnd :: Card
    theEnd =
      Card
        "The End"
        "You're out of cards, hurt yourself for 10."
        "the_end.svg"
        $ \w' -> (betaRaw $ hurt 10 w') >> betaNull


bounceAll :: WhichPlayer -> AlphaProgram ()
bounceAll w = do
  (ours, theirs) <- partition (owned w) <$> getStack
  setStack theirs
  let oursCards = (\(StackCard _ c) -> c) <$> ours
  modHand w $ \h -> h ++ oursCards


incPasses :: Passes -> Passes
incPasses NoPass  = OnePass
incPasses OnePass = NoPass


resetPasses :: AlphaProgram ()
resetPasses = setPasses NoPass


swapTurn :: AlphaProgram ()
swapTurn = do
  modTurn other
  modPasses incPasses


addToHand :: WhichPlayer -> Card -> AlphaProgram ()
addToHand w c = modHand w (\h -> h ++ [c])


handFull :: WhichPlayer -> AlphaProgram Bool
handFull w = do
  handLength <- length <$> getHand w
  return $ handLength >= maxHandLength


alphaEffI :: Model -> Alpha a -> (Model, a)
alphaEffI m (GetGen f)      = (m, f $ model_gen m)
alphaEffI m (GetPasses f)   = (m, f $ model_passes m)
alphaEffI m (GetStack f)    = (m, f $ model_stack m)
alphaEffI m (GetTurn f)     = (m, f $ model_turn m)
alphaEffI m (GetDeck w f)   = (m, f . pmodel_deck $ getPmodel w m)
alphaEffI m (GetHand w f)   = (m, f . pmodel_hand $ getPmodel w m )
alphaEffI m (GetLife w f)   = (m, f . pmodel_life $ getPmodel w m)
alphaEffI m (SetGen g n)    = (m { model_gen = g }, n)
alphaEffI m (SetDeck w d n) = (modPmodel (\pm -> pm { pmodel_deck = d }) w m, n)
alphaEffI m (SetHand w h n) =
                              let
                                newHand :: Hand
                                newHand = take maxHandLength h
                              in
                                (modPmodel (\pm -> pm { pmodel_hand = newHand }) w m, n)
alphaEffI m (SetLife w l n) =
                              let
                                newLife :: Life
                                newLife = max 0 . min maxLife $ l
                              in
                                (modPmodel (\pm -> pm { pmodel_life = newLife }) w m, n)
alphaEffI m (SetPasses p n) = (m { model_passes = p }, n)
alphaEffI m (SetStack s n)  = (m { model_stack = s }, n)
alphaEffI m (SetTurn t n)   = (m { model_turn = t }, n)


alphaI :: BetaProgram a -> AlphaProgram a
alphaI (Free (BetaRaw p n))         = p             >>  alphaI n
alphaI (Free (BetaSlash d w n))     = hurt d w      >>  alphaI n
alphaI (Free (BetaHeal h w n))      = heal h w      >>  alphaI n
alphaI (Free (BetaDraw w n))        = draw w        >>  alphaI n
alphaI (Free (BetaAddToHand w c n)) = addToHand w c >>  alphaI n
alphaI (Free (BetaGetGen f))        = getGen        >>= alphaI . f
alphaI (Free (BetaGetLife w f))     = getLife w     >>= alphaI . f
alphaI (Free (BetaGetHand w f))     = getHand w     >>= alphaI . f
alphaI (Free (BetaGetDeck w f))     = getDeck w     >>= alphaI . f
alphaI (Free (BetaGetStack f))      = getStack      >>= alphaI . f
alphaI (Free (BetaNull n))          = alphaI n
alphaI (Pure x)                     = Pure x


-- Animation DSL
data AnimDSL a
  = AnimNull a
  | AnimSlash WhichPlayer Life a
  | AnimHeal WhichPlayer a
  | AnimDraw WhichPlayer a
  deriving (Functor)


type AlphaAnimProgram = Free (Sum Alpha AnimDSL)


animI :: Beta a -> ((AlphaProgram a) -> AlphaAnimProgram a)
animI (BetaSlash d w _)     = \a -> (toLeft a) <* (toRight . liftF $ AnimSlash w d ())
animI (BetaHeal _ w _)      = \a -> (toLeft a) <* (toRight . liftF $ AnimHeal w ())
animI (BetaNull _)          = \a -> (toLeft a) <* (toRight . liftF $ AnimNull ())
animI (BetaAddToHand w _ _) = drawAnim w
animI (BetaDraw w _)        = drawAnim w
animI _                     = toLeft


drawAnim :: WhichPlayer -> ((AlphaProgram a) -> AlphaAnimProgram a)
drawAnim w alpha =
  do
    handLength <- length <$> toLeft (getHand w)
    final <- toLeft alpha
    when (handLength < maxHandLength) $
      toRight . liftF $ AnimDraw w ()
    return final


animate :: AnimDSL a -> Maybe CardAnim
animate (AnimNull _)      = Nothing
animate (AnimSlash w d _) = Just $ Slash w d
animate (AnimHeal w _)    = Just . Heal $ w
animate (AnimDraw w _)    = Just . Draw $ w


animNext :: AnimDSL a -> a
animNext (AnimNull n)      = n
animNext (AnimSlash _ _ n) = n
animNext (AnimHeal _ n)    = n
animNext (AnimDraw _ n)    = n


-- Logging DSL
data LogDSL a
  = Log String a
  deriving (Functor)


type LogProgram a = Free LogDSL a


makeFree ''LogDSL


logI :: Alpha a -> LogProgram ()
logI (GetGen _)      = log $ printf "Get gen"
logI (GetDeck w _)   = log $ printf "Get deck %s" (show w)
logI (GetHand w _)   = log $ printf "Get hand %s" (show w)
logI (GetLife w _)   = log $ printf "Get life %s" (show w)
logI (GetPasses _)   = log $ printf "Get passes"
logI (GetStack _)    = log $ printf "Get stack"
logI (GetTurn _)     = log $ printf "Get turn"
logI (SetGen g _)    = log $ printf "Set gen %s"     (show g)
logI (SetDeck w d _) = log $ printf "Set deck %s %s" (show w) (show d)
logI (SetHand w h _) = log $ printf "Set hand %s %s" (show w) (show h)
logI (SetLife w l _) = log $ printf "Set life %s %s" (show w) (show l)
logI (SetPasses p _) = log $ printf "Set passes %s"  (show p)
logI (SetStack s _)  = log $ printf "Set stack %s"   (show s)
logI (SetTurn t _)   = log $ printf "Set turn %s"    (show t)


type AlphaLogProgram = Free (Sum Alpha LogDSL)


alphaDecorateLog :: ∀ a . Alpha a -> AlphaLogProgram a
alphaDecorateLog x =
  let
    alpha   = liftF x :: AlphaProgram a
    logging = logI x  :: LogProgram ()
  in
    toLeft alpha <* toRight logging


liftAlphaAnim :: ∀ a . Sum Alpha AnimDSL a -> AlphaLogAnimProgram a
liftAlphaAnim (InL alpha) = toLeft $ alphaDecorateLog alpha
liftAlphaAnim (InR anim)  = toRight $ liftF anim


type AlphaLogAnimProgram = Free (Sum (Sum Alpha LogDSL) AnimDSL)


betaI :: ∀ a . Beta a -> AlphaLogAnimProgram a
betaI x =
  let
    alpha     = alphaI $ liftF x :: AlphaProgram a
    embedAnim = animI x          :: (AlphaProgram a -> AlphaAnimProgram a)
  in
    foldFree liftAlphaAnim $ embedAnim alpha


execute :: Model -> AlphaLogAnimProgram a -> (Model, a, String, [(Model, Maybe CardAnim)])
execute = execute' "" []
  where
    execute' :: String -> [(Model, Maybe CardAnim)] -> Model -> AlphaLogAnimProgram a -> (Model, a, String, [(Model, Maybe CardAnim)])
    execute' s a m (Pure x) =
      (m, x, s, a)
    execute' s a m (Free (InR anim)) =
      execute' s (a ++ [(m, animate anim)]) m (animNext anim)
    execute' s a m (Free (InL (InL p)))  =
      (uncurry (execute' s a)) (alphaEffI m p)
    execute' s a m (Free (InL (InR (Log l n)))) =
      execute' (s ++ l ++ "\n") a m n


effI :: Model -> AlphaProgram a -> (Model, a)
effI m (Pure x) = (m, x)
effI m (Free p) = (uncurry effI) (alphaEffI m p)


modI :: Model -> AlphaProgram () -> Model
modI m p = fst $ effI m p


evalI :: Model -> AlphaProgram a -> a
evalI m p = snd $ effI m p
