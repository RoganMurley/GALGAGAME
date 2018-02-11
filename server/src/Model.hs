module Model where

import Data.Aeson (ToJSON(..), (.=), object)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Text (Text, toUpper)
import Mirror (Mirror(..))
import Player (WhichPlayer(..), other)
import Util (Gen)

import {-# SOURCE #-} qualified DSL.Beta.DSL as Beta


data Card = Card
  { card_name :: Text
  , card_desc :: Text
  , card_img  :: Text
  , card_eff  :: WhichPlayer -> Beta.Program ()
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


data CardAnim
  = Slash WhichPlayer Int
  | Heal WhichPlayer
  | Draw WhichPlayer
  | Reverse
  | Obliterate
  | Play WhichPlayer Card
  deriving (Show, Eq)


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
  toJSON Reverse =
    object
    [
     "player" .= PlayerA
    , "anim"  .= ("reverse" :: Text)
    ]
  toJSON Obliterate =
    object
    [
     "player" .= PlayerA
    , "anim"  .= ("obliterate" :: Text)
    ]
  toJSON (Play w c) =
    object
    [
     "player" .= w
    , "anim"  .= ("play" :: Text, c)
    ]


instance Mirror CardAnim where
  mirror (Slash w d) = Slash (other w) d
  mirror (Heal w)    = Heal  (other w)
  mirror (Draw w)    = Draw  (other w)
  mirror Reverse     = Reverse
  mirror Obliterate  = Obliterate
  mirror (Play w c)  = Play (other w) c


maxHandLength :: Int
maxHandLength = 6


maxLife :: Life
maxLife = 50


changeOwner :: StackCard -> StackCard
changeOwner = mirror


owner :: WhichPlayer -> StackCard -> Bool
owner w (StackCard o _) = w == o


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
