module ResolveData where

import CardAnim (CardAnim)
import Data.Aeson (ToJSON(..), (.=), object)
import Life (Life)
import Mirror (Mirror(..))
import ModelDiff (ModelDiff)

data ResolveData =
  ResolveData
    { resolveData_diff       :: ModelDiff
    , resolveData_anim       :: Maybe CardAnim
    , resolveData_animDamage :: (Life, Life)
    }
  deriving (Show, Eq)


instance Mirror ResolveData where
  mirror (ResolveData diff anim dmg) =
    ResolveData (mirror diff) (mirror <$> anim) (mirror dmg)


instance ToJSON ResolveData where
  toJSON (ResolveData diff anim damage) =
    object [
      "diff"      .= diff
    , "anim"      .= anim
    , "damage"    .= damage
    ]

resolveAnim :: CardAnim -> ResolveData
resolveAnim anim = ResolveData mempty (Just anim) (0, 0)
