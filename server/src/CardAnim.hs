module CardAnim where

import Bounce (CardBounce)
import Card (Card(..))
import Data.Aeson (ToJSON(..), (.=), object)
import Data.Text (Text)
import Life (Life)
import Mirror (Mirror(..))
import Player (WhichPlayer(..), other)
import StackCard (StackCard)


data CardAnim
  = Slash WhichPlayer Life
  | Heal WhichPlayer Life
  | Draw WhichPlayer
  | Bite WhichPlayer Life
  | Reflect
  | Reverse
  | Hubris
  | Play WhichPlayer Card Int
  | Transmute StackCard StackCard
  | Mill WhichPlayer Card
  | GameEnd (Maybe WhichPlayer)
  | Rotate
  | Windup
  | Fabricate StackCard
  | Bounce [CardBounce]
  | NewRound
  | EndTurnAnim WhichPlayer
  deriving (Show, Eq)


type SfxUrl = Text
type ShaderName = Text


instance ToJSON CardAnim where
  toJSON (Slash w d) =
    object
    [ "player" .= w
    , "anim"   .= ("slash" :: Text, d)
    ]
  toJSON (Heal w h) =
    object
    [ "player" .= w
    , "anim"   .= ("heal" :: Text, h)
    ]
  toJSON (Draw w) =
    object
    [ "player" .= w
    , "anim"   .= ("draw" :: Text)
    ]
  toJSON (Bite w d) =
    object
    [ "player" .= w
    , "anim"   .= ("bite" :: Text, d)
    ]
  toJSON Reflect =
    object
    [ "player" .= PlayerA
    , "anim"   .= ("reflect" :: Text)
    ]
  toJSON Reverse =
    object
    [ "player" .= PlayerA
    , "anim"   .= ("reverse" :: Text)
    ]
  toJSON Hubris =
    object
    [ "player" .= PlayerA
    , "anim"   .= ("hubris" :: Text)
    ]
  toJSON (Play w c i) =
    object
    [ "player" .= w
    , "anim"   .= ("play" :: Text, c, i)
    ]
  toJSON (Transmute ca cb) =
    object
    [ "player" .= PlayerA
    , "anim"   .= ("transmute" :: Text, ca, cb)
    ]
  toJSON (Mill w c) =
    object
    [ "player" .= w
    , "anim"   .= ("mill" :: Text, c)
    ]
  toJSON (GameEnd w) =
    object
    [ "player" .= PlayerA
    , "anim"   .= ("gameEnd" :: Text, w)
    ]
  toJSON Rotate =
    object
    [ "player" .= PlayerA
    , "anim"   .= ("rotate" :: Text)
    ]
  toJSON Windup =
    object
    [ "player" .= PlayerA
    , "anim"   .= ("windup" :: Text)
    ]
  toJSON (Fabricate stackCard) =
    object
    [ "player" .= PlayerA
    , "anim"   .= ("fabricate" :: Text, stackCard)
    ]
  toJSON (Bounce b) =
    object
    [ "player" .= PlayerA
    , "anim"   .= ("bounce" :: Text, b)
    ]
  toJSON NewRound =
    object
    [ "player" .= PlayerA
    , "anim"   .= ("newRound" :: Text)
    ]
  toJSON (EndTurnAnim w) =
    object
    [ "player" .= w
    , "anim"   .= ("endTurn" :: Text)
    ]


instance Mirror CardAnim where
  mirror (Slash w d)       = Slash (other w) d
  mirror (Heal w h)        = Heal (other w) h
  mirror (Draw w)          = Draw  (other w)
  mirror (Bite w d)        = Bite (other w) d
  mirror Reflect           = Reflect
  mirror Reverse           = Reverse
  mirror Hubris            = Hubris
  mirror (Play w c i)      = Play (other w) c i
  mirror (Transmute ca cb) = Transmute (mirror ca) (mirror cb)
  mirror (GameEnd w)       = GameEnd (other <$> w)
  mirror (Mill w c)        = Mill (other w) c
  mirror Rotate            = Rotate
  mirror Windup            = Windup
  mirror (Fabricate c)     = Fabricate (mirror c)
  mirror (Bounce b)        = Bounce b
  mirror NewRound          = NewRound
  mirror (EndTurnAnim w)   = EndTurnAnim (other w)


cardAnimDamage :: CardAnim -> (Life, Life)
cardAnimDamage anim =
  let
      wrap :: WhichPlayer -> Life -> ( Life, Life )
      wrap w d =
          case w of
              PlayerA ->
                  ( d, 0 )

              PlayerB ->
                  ( 0, d )
  in
  case anim of
      Heal w h ->
          wrap w h

      Slash w d ->
          wrap w (-d)

      Bite w d ->
          wrap w (-d)

      _ ->
          ( 0, 0 )
