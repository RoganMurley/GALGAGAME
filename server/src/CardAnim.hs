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
  | Reverse
  | Obliterate
  | Play WhichPlayer Card
  | Transmute StackCard StackCard
  | Overdraw WhichPlayer
  | GameEnd (Maybe WhichPlayer)
  | Adhoc WhichPlayer ShaderName SfxUrl
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
  toJSON (Play w c) =
    object
    [ "player" .= w
    , "anim"  .= ("play" :: Text, c, 0 :: Int)
    ]
  toJSON (Transmute ca cb) =
    object
    [ "player" .= PlayerA
    , "anim"  .= ("transmute" :: Text, ca, cb)
    ]
  toJSON (Overdraw w) =
    object
    [ "player" .= w
    , "anim"  .= ("overdraw" :: Text)
    ]
  toJSON (GameEnd w) =
    object
    [ "player" .= PlayerA
    , "anim"  .= ("gameEnd" :: Text, w)
    ]
  toJSON (Adhoc w n s) =
    object
    [ "player" .= w
    , "anim"  .= ("adhoc" :: Text, n, s)
    ]


instance Mirror CardAnim where
  mirror (Slash w d)       = Slash (other w) d
  mirror (Heal w)          = Heal  (other w)
  mirror (Draw w)          = Draw  (other w)
  mirror (Bite w d)        = Bite (other w) d
  mirror Reverse           = Reverse
  mirror Obliterate        = Obliterate
  mirror (Play w c)        = Play (other w) c
  mirror (Transmute ca cb) = Transmute (mirror ca) (mirror cb)
  mirror (GameEnd w)       = GameEnd (other <$> w)
  mirror (Overdraw w)      = Overdraw (other w)
  mirror (Adhoc w n s)     = Adhoc (other w) n s
