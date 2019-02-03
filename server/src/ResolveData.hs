module ResolveData where

import CardAnim (CardAnim)
import Data.Aeson (ToJSON(..), (.=), object)
import Life (Life)
import Mirror (Mirror(..))
import ModelDiff (ModelDiff)
import StackCard (StackCard)

data ResolveData =
  ResolveData
    { resolveData_diff       :: ModelDiff
    , resolveData_anim       :: Maybe CardAnim
    , resolveData_animDamage :: (Life, Life)
    , resolveData_stackCard  :: Maybe StackCard
    }
  deriving (Show, Eq)


instance Mirror ResolveData where
  mirror (ResolveData diff anim dmg stackCard) =
    ResolveData (mirror diff) (mirror <$> anim) (mirror dmg) (mirror stackCard)


instance ToJSON ResolveData where
  toJSON (ResolveData diff anim damage stackCard) =
    object [
      "diff"      .= diff
    , "anim"      .= anim
    , "damage"    .= damage
    , "stackCard" .= stackCard
    ]

resolveAnim :: CardAnim -> ResolveData
resolveAnim anim = ResolveData mempty (Just anim) (0, 0) Nothing
