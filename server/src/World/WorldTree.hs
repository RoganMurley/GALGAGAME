module World.WorldTree where

import Data.Set (Set)
import World.Pos (Pos)
import World.Tarot (Tarot)
import World.WorldKey (WorldKey(..))
import World.WorldProgress (WorldProgress(..))

import qualified Data.Set as Set
import qualified World.Tarot as T


data WorldTree =
  WorldTree
  { worldtree_start         :: WorldNode
  , worldtree_crown         :: WorldNode
  , worldtree_understanding :: WorldNode
  , worldtree_wisdom        :: WorldNode
  , worldtree_severity      :: WorldNode
  , worldtree_mercy         :: WorldNode
  , worldtree_splendor      :: WorldNode
  , worldtree_victory       :: WorldNode
  , worldtree_beauty        :: WorldNode
  , worldtree_foundation    :: WorldNode
  , worldtree_kingdom       :: WorldNode
  }

data WorldNode =
  WorldNode
  { worldnode_edges :: [Edge]
  }


data Edge =
  Edge
  { edge_tarot :: WorldProgress -> Tarot
  , edge_key   :: WorldKey
  }


mainKeys :: Set WorldKey
mainKeys =
  Set.fromList
    [ Crown
    , Understanding
    , Wisdom
    , Severity
    , Mercy
    , Splendor
    , Victory
    , Beauty
    , Foundation
    , Kingdom
    ]


worldTree :: WorldTree
worldTree =
  WorldTree
  { worldtree_start         = start
  , worldtree_crown         = crown
  , worldtree_understanding = understanding
  , worldtree_wisdom        = wisdom
  , worldtree_severity      = severity
  , worldtree_mercy         = mercy
  , worldtree_splendor      = splendor
  , worldtree_victory       = victory
  , worldtree_beauty        = beauty
  , worldtree_foundation    = foundation
  , worldtree_kingdom       = kingdom
  }
  where
  start         = WorldNode [Edge T.tarotBeginning Crown]
  crown         = WorldNode [Edge T.tarotMagician Understanding, Edge T.tarotFool Wisdom, Edge T.tarotPriestess Beauty]
  understanding = WorldNode [Edge T.tarotMagician Crown, Edge T.tarotEmpress Wisdom, Edge T.tarotChariot Severity, Edge T.tarotLovers Beauty]
  wisdom        = WorldNode [Edge T.tarotFool Crown, Edge T.tarotEmpress Understanding, Edge T.tarotHierophant Mercy, Edge T.tarotEmperor Beauty]
  severity      = WorldNode [Edge T.tarotChariot Understanding, Edge T.tarotJustice Mercy, Edge T.tarotStrength Beauty, Edge T.tarotHanged Splendor]
  mercy         = WorldNode [Edge T.tarotHierophant Wisdom, Edge T.tarotJustice Severity, Edge T.tarotHermit Beauty, Edge T.tarotWheel Victory]
  splendor      = WorldNode [Edge T.tarotHanged Severity, Edge T.tarotDevil Beauty, Edge T.tarotSun Foundation, Edge T.tarotJudgement Kingdom]
  victory       = WorldNode [Edge T.tarotWheel Mercy, Edge T.tarotDeath Beauty, Edge T.tarotStar Foundation, Edge T.tarotMoon Kingdom]
  beauty        = WorldNode [Edge T.tarotPriestess Crown, Edge T.tarotLovers Understanding, Edge T.tarotEmperor Wisdom, Edge T.tarotStrength Severity, Edge T.tarotHermit Mercy, Edge T.tarotDevil Splendor, Edge T.tarotDeath Victory, Edge T.tarotTemperance Foundation]
  foundation    = WorldNode [Edge T.tarotTemperance Beauty, Edge T.tarotSun Splendor, Edge T.tarotStar Victory, Edge T.tarotWorld Kingdom]
  kingdom       = WorldNode [Edge T.tarotJudgement Splendor, Edge T.tarotWorld Foundation, Edge T.tarotMoon Victory]


getEdges :: WorldKey ->  WorldTree -> WorldNode
getEdges Start         = worldtree_start
getEdges Crown         = worldtree_crown
getEdges Understanding = worldtree_understanding
getEdges Wisdom        = worldtree_wisdom
getEdges Severity      = worldtree_severity
getEdges Mercy         = worldtree_mercy
getEdges Splendor      = worldtree_splendor
getEdges Victory       = worldtree_victory
getEdges Beauty        = worldtree_beauty
getEdges Foundation    = worldtree_foundation
getEdges Kingdom       = worldtree_kingdom


getAdjEdges :: WorldKey -> [Edge]
getAdjEdges key = worldnode_edges $ getEdges key worldTree


getEdgePositions :: WorldKey -> Set WorldKey -> [(Pos, Pos)]
getEdgePositions key edgeKeys = zip (repeat startPosition) endPositions
  where
    startPosition :: Pos
    startPosition = getPosition key
    endPositions :: [Pos]
    endPositions = getPosition <$> (Set.toList $ edgeKeys)


lockedEdgePositions :: [(Pos, Pos)]
lockedEdgePositions = Set.toList $ Set.unions lockedEdges
  where
    lockedEdges :: [Set (Pos, Pos)]
    lockedEdges = getLocked <$> (Set.toList mainKeys)
    getLocked :: WorldKey -> Set (Pos, Pos)
    getLocked key = Set.fromList $ getEdgePositions key (Set.fromList $ edge_key <$> getAdjEdges key)


gap :: Float
gap = 0.08
level0 :: Float
level0 = 0.33
level1 :: Float
level1 = level0 + gap
level2 :: Float
level2 = level1 + gap
level3 :: Float
level3 = level2 + gap
levelBeauty0 :: Float
levelBeauty0 = level2 + 0.5 * gap
levelBeauty1 :: Float
levelBeauty1 = levelBeauty0 + gap
levelBeauty2 :: Float
levelBeauty2 = levelBeauty1 + gap


getPosition :: WorldKey -> Pos
getPosition Start         = (0.5, level0)
getPosition Crown         = (0.5, level0)
getPosition Understanding = (0.4, level1)
getPosition Wisdom        = (0.6, level1)
getPosition Severity      = (0.4, level2)
getPosition Mercy         = (0.6, level2)
getPosition Splendor      = (0.4, level3)
getPosition Victory       = (0.6, level3)
getPosition Beauty        = (0.5, levelBeauty0)
getPosition Foundation    = (0.5, levelBeauty1)
getPosition Kingdom       = (0.5, levelBeauty2)
