{-# LANGUAGE NoImplicitPrelude #-}
module Space
  ( Area (..)
  , spaceInArea
  , simpleAreaFromSpace
  , Space (..)
  , SpaceType (..)
  , Route (..)
  , RouteType (..)
  , ConvoyPath (..)
  , cpWithoutDuplicates
  ) where

import Error
import Util
import RIO
import qualified RIO.List as L ( intercalate )
import qualified RIO.NonEmpty as NE ( toList )
import qualified RIO.Set as S ( member, singleton, toList )

-- Spaces and related functions
data SpaceType = Land | Ocean | Coast deriving (Eq, Ord, Show)

data Space = Space { spaceName :: String, spaceType :: SpaceType }

instance Show Space where
  show s = spaceName s <> ", " <> (show . spaceType) s

instance Eq Space where
  (==) s1 s2 = spaceName s1 == spaceName s2

instance Ord Space where
  (<=) s1 s2 = spaceName s1 <= spaceName s2

data RouteType = ArmyOnly | FleetOnly | BothUnits | ConvoyOnly deriving (Eq, Ord)

instance Show RouteType where
  show ArmyOnly = "[A]"
  show FleetOnly = "[F]"
  show BothUnits = "[B]"
  show ConvoyOnly = "[C]"

-- neighbors aka "routes" are represented as unordered pairs
-- e.g. (a,b) is equivalent to (b, a)
data Route = Route { space1 :: Space, space2 :: Space }

instance Show Route where
  show r = let spn1 = spaceName (space1 r) 
               spn2 = spaceName (space2 r) 
           in min spn1 spn2 <> "-" <> max spn1 spn2-- <> " " <> show (routeType r)


maxSpace :: Route -> Space
maxSpace = max <$> space1 <*> space2
minSpace = min <$> space1 <*> space2

instance Eq Route where
  (==) route1 route2 = maxSpace route1 == maxSpace route2
                    && minSpace route1 == minSpace route2

-- order does not depend on which component has which space
instance Ord Route where
  (<=) route1 route2 = minSpace route1 < minSpace route2
    || (minSpace route1 == minSpace route2 && maxSpace route1 <= maxSpace route2)

-- A specific type for convoy path
data ConvoyPath = ConvoyPath { cpVia :: NonEmpty Space
                             , cpTo :: Space } deriving (Eq, Ord)

showMid :: NonEmpty Space -> String
showMid (s1 :| s2) = unwords . fmap spaceName $ s1:s2

instance Show ConvoyPath where
  show (ConvoyPath cpvia to) = "via " <> showMid cpvia <> " to " <> spaceName to

viaWithoutDuplicates cp = safeToSet (NE.toList . cpVia $ cp) *> return cp

endPointWithoutDuplicates spcFrom cp = case spcFrom == cpTo cp of
  True  -> ValidationError "The convoy path cannot start from and end to the same space."
  False -> Valid cp

cpWithoutDuplicates :: Space -> ConvoyPath -> Validated ConvoyPath
cpWithoutDuplicates spc = endPointWithoutDuplicates spc >=> viaWithoutDuplicates

-- Area is a concept for places with multiple coasts
-- so that in some sense there are multiple spaces in the same area
-- (but only one of them can be occupied at the same time and attacking
--  to one of them also attacks to the other spaces and so forth)
data Area = Area { areaName :: String, areaMembers :: Set Space, areaImplicit :: Bool } deriving (Eq, Ord)

instance Show Area where
  show (Area n _ True)     = n
  show (Area n memb False) = n <> ": " <> (L.intercalate "~" . fmap spaceName . S.toList $ memb)

simpleAreaFromSpace :: Space -> Area
simpleAreaFromSpace spc = Area (spaceName spc) (S.singleton spc) True

spaceInArea :: Space -> Area -> Bool
spaceInArea spc (Area _ sa _) = S.member spc sa

