module CardAnim where

import Card (Card(..))
import Data.Aeson (ToJSON(..), (.=), object)
import Data.Text (Text)
import Life (Life)
import Mirror (Mirror(..))
import Player (WhichPlayer(..), other)
import StackCard (StackCard)


data CardAnim
  = Slash WhichPlayer Life
  | Heal WhichPlayer
  | Draw WhichPlayer
  | Bite WhichPlayer Life
  | Reflect
  | Reverse
  | Obliterate
  | Play WhichPlayer Card Int
  | Transmute StackCard StackCard
  | Overdraw WhichPlayer Card
  | GameEnd (Maybe WhichPlayer)
  | Rotate
  | Windup
  deriving (Show, Eq)


type SfxUrl = Text
type ShaderName = Text


instance ToJSON CardAnim where
  toJSON (Slash w d) =
    object
    [ "player" .= w
    , "anim"  .= ("slash" :: Text, d)
    ]
  toJSON (Heal w) =
    object
    [ "player" .= w
    , "anim"  .= ("heal" :: Text)
    ]
  toJSON (Draw w) =
    object
    [ "player" .= w
    , "anim"  .= ("draw" :: Text)
    ]
  toJSON (Bite w d) =
    object
    [ "player" .= w
    , "anim"  .= ("bite" :: Text, d)
    ]
  toJSON Reflect =
    object
    [ "player" .= PlayerA
    , "anim"  .= ("reflect" :: Text)
    ]
  toJSON Reverse =
    object
    [ "player" .= PlayerA
    , "anim"  .= ("reverse" :: Text)
    ]
  toJSON Obliterate =
    object
    [ "player" .= PlayerA
    , "anim"  .= ("obliterate" :: Text)
    ]
  toJSON (Play w c i) =
    object
    [ "player" .= w
    , "anim"  .= ("play" :: Text, c, i)
    ]
  toJSON (Transmute ca cb) =
    object
    [ "player" .= PlayerA
    , "anim"  .= ("transmute" :: Text, ca, cb)
    ]
  toJSON (Overdraw w c) =
    object
    [ "player" .= w
    , "anim"  .= ("overdraw" :: Text, c)
    ]
  toJSON (GameEnd w) =
    object
    [ "player" .= PlayerA
    , "anim"  .= ("gameEnd" :: Text, w)
    ]
  toJSON Rotate =
    object
    [ "player" .= PlayerA
    , "anim"  .= ("rotate" :: Text)
    ]
  toJSON Windup =
    object
    [ "player" .= PlayerA
    , "anim"  .= ("windup" :: Text)
    ]


instance Mirror CardAnim where
  mirror (Slash w d)       = Slash (other w) d
  mirror (Heal w)          = Heal  (other w)
  mirror (Draw w)          = Draw  (other w)
  mirror (Bite w d)        = Bite (other w) d
  mirror Reflect           = Reflect
  mirror Reverse           = Reverse
  mirror Obliterate        = Obliterate
  mirror (Play w c i)      = Play (other w) c i
  mirror (Transmute ca cb) = Transmute (mirror ca) (mirror cb)
  mirror (GameEnd w)       = GameEnd (other <$> w)
  mirror (Overdraw w c)    = Overdraw (other w) c
  mirror Rotate            = Rotate
  mirror Windup            = Windup
