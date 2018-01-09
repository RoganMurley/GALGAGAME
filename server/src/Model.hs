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

import Mirror (Mirror(..))
import Player (WhichPlayer(..), other)
import Util (Gen)


data Card = Card
  { card_name :: Text
  , card_desc :: Text
  , card_img  :: Text
  , card_snd  :: Text
  , card_anim :: Maybe CardAnim
  , card_eff  :: WhichPlayer -> BetaProgram ()
  }


instance Eq Card where
  (Card n1 d1 i1 _ _ _) == (Card n2 d2 i2 _ _ _) =
    n1 == n2 && d1 == d2 && i1 == i2


instance Show Card where
  show = cs . card_name


instance ToJSON Card where
  toJSON (Card name desc imageURL sfxURL anim _) =
    object
      [
        "name"     .= name
      , "desc"     .= desc
      , "imageURL" .= imageURL
      , "sfxURL"   .= sfxURL
      , "anim"     .= anim
      ]


instance ToJSON CardAnim where
  toJSON Slash      = "slash"
  toJSON Heal       = "heal"
  toJSON Obliterate = "obliterate"


data CardAnim =
    Slash
  | Heal
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
  | BetaDraw WhichPlayer n
  deriving (Functor)


type AlphaProgram a = Free Alpha a
type BetaProgram a = Free Beta a


makeFree ''Alpha
makeFree ''Beta


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
        "feint.wave"
        Nothing
        $ betaRaw . (hurt 10)


bounceAll :: WhichPlayer -> AlphaProgram ()
bounceAll w = do
  (ours, theirs) <- partition (owned w) <$> getStack
  setStack theirs
  let oursCards = (\(StackCard _ c) -> c) <$> ours
  modHand w $ (++) oursCards


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
addToHand w c = modHand w ((:) c)


handFull :: WhichPlayer -> AlphaProgram Bool
handFull w = do
  handLength <- length <$> getHand w
  return $ handLength >= maxHandLength



effI :: Model -> AlphaProgram a -> (Model, a)
effI m (Free (GetGen f))      = (effI m) . f . model_gen $ m
effI m (Free (GetDeck w f))   = (effI m) . f . pmodel_deck $ getPmodel w m
effI m (Free (GetHand w f))   = (effI m) . f . pmodel_hand $ getPmodel w m
effI m (Free (GetLife w f))   = (effI m) . f . pmodel_life $ getPmodel w m
effI m (Free (GetPasses f))   = (effI m) . f . model_passes $ m
effI m (Free (GetStack f))    = (effI m) . f . model_stack $ m
effI m (Free (GetTurn f))     = (effI m) . f . model_turn $ m
effI m (Free (SetGen g n))    = effI (m { model_gen = g }) n
effI m (Free (SetDeck w d n)) = effI (modPmodel (\pm -> pm { pmodel_deck = d }) w m) n
effI m (Free (SetHand w h n)) = effI (modPmodel (\pm -> pm { pmodel_hand = reverse . (take maxHandLength) $ reverse h }) w m) n
effI m (Free (SetLife w l n)) = effI (modPmodel (\pm -> pm { pmodel_life = l }) w m) n
effI m (Free (SetPasses p n)) = effI (m { model_passes = p }) n
effI m (Free (SetStack s n))  = effI (m { model_stack = s }) n
effI m (Free (SetTurn t n))   = effI (m { model_turn = t }) n
effI m (Pure x)               = (m, x)


modI :: Model -> AlphaProgram a -> Model
modI m p = fst $ effI m p


evalI :: Model -> AlphaProgram a -> a
evalI m p = snd $ effI m p


alphaI :: BetaProgram a -> AlphaProgram a
alphaI (Free (BetaRaw p n))  = p      >> alphaI n
alphaI (Free (BetaDraw w n)) = draw w >> alphaI n
alphaI (Pure x)              = Pure x


data LogDSL a
  = Log String a
  deriving (Functor)


type LogProgram a = Free LogDSL a


makeFree ''LogDSL


logI :: Alpha a -> LogProgram ()
logI (GetGen _)      = log "Get gen"
logI (GetDeck _ _)   = log "Get deck"
logI (GetHand _ _)   = log "Get hand"
logI (GetLife _ _)   = log "Get life"
logI (GetPasses _)   = log "Get passes"
logI (GetStack _)    = log "Get stack"
logI (GetTurn _)     = log "Get turn"
logI (SetGen _ _)    = log "Set gen"
logI (SetDeck _ _ _) = log "Set deck"
logI (SetHand _ _ _) = log "Set hand"
logI (SetLife _ _ _) = log "Set life"
logI (SetPasses _ _) = log "Set passes"
logI (SetStack _ _)  = log "Set stack"
logI (SetTurn _ _)   = log "Set turn"


type Program = Free (Sum Alpha LogDSL)


superI :: Alpha a -> Program a
superI op = toRight (logI op) *> toLeft (liftF op)


progI :: Model -> Alpha a -> (Model, a)
progI m (GetGen f)      = (m, f $ model_gen m)
progI m (GetPasses f)   = (m, f $ model_passes m)
progI m (GetStack f)    = (m, f $ model_stack m)
progI m (GetTurn f)     = (m, f $ model_turn m)
progI m (GetDeck w f)   = (m, f . pmodel_deck $ getPmodel w m)
progI m (GetHand w f)   = (m, f . pmodel_hand $ getPmodel w m )
progI m (GetLife w f)   = (m, f . pmodel_life $ getPmodel w m)
progI m (SetGen g n)    = (m { model_gen = g }, n)
progI m (SetDeck w d n) = (modPmodel (\pm -> pm { pmodel_deck = d }) w m, n)
progI m (SetHand w h n) = (modPmodel (\pm -> pm { pmodel_hand = reverse . (take maxHandLength) $ reverse h }) w m, n)
progI m (SetLife w l n) = (modPmodel (\pm -> pm { pmodel_life = l }) w m, n)
progI m (SetPasses p n) = (m { model_passes = p }, n)
progI m (SetStack s n)  = (m { model_stack = s }, n)
progI m (SetTurn t n)   = (m { model_turn = t }, n)


execute :: Model -> Program a -> (Model, a, String)
execute = execute' ""
  where
    execute' :: String -> Model -> Program a -> (Model, a, String)
    execute' s m (Free (InL p))  =
      let
        (model, program) = progI m p
      in
                                          execute' (s ++ "<PROG>") model program
    execute' s m (Free (InR (Log l n))) =
                                          execute' (s ++ l ++ "\n") m n
    execute' s m (Pure x) =
                                          (m, x, s)


toLeft :: (Functor f, Functor g) => Free f a -> Free (Sum f g) a
toLeft (Free f) = Free $ InL (toLeft <$> f)
toLeft (Pure x) = Pure x


toRight :: (Functor f, Functor g) => Free g a -> Free (Sum f g) a
toRight (Free f) = Free $ InR (toRight <$> f)
toRight (Pure x) = Pure x


prog :: Program Life
prog = foldFree superI $ do
  setLife PlayerA 10
  life <- getLife PlayerA
  when (life < 10) $ setLife PlayerA 999
  getLife PlayerA
