{-# LANGUAGE NoImplicitPrelude #-}
module Spaces
  ( Area (..)
  , spaceInArea
  , simpleAreaFromSpace
  , Space (..)
  , SpaceType (..)
  , Route (..)
  , RouteType (..)
  , routeWithoutDuplicates
  , ConvoyPath (..)
  , cpWithoutDuplicates
  ) where

import Errors
import Utils
import RIO
import RIO.Prelude ((>=>))
import RIO.List ( intercalate )
import RIO.NonEmpty ( (<|) )
import qualified RIO.NonEmpty as NE ( toList )
import qualified RIO.Set as S ( member, singleton, toAscList )

-- Spaces and related functions
data SpaceType = Land | Ocean | Coast deriving (Eq, Ord, Show)

data Space = Space { spaceName :: String, spaceType :: SpaceType }

instance Show Space where
  show s = spaceName s ++ ", " ++ (show . spaceType) s

instance Eq Space where
  (==) s1 s2 = spaceName s1 == spaceName s2

instance Ord Space where
  (<=) s1 s2 = spaceName s1 <= spaceName s2

-- neighbors aka "routes" are represented as unordered pairs
-- e.g. (a,b) is equivalent to (b, a)
data RouteType = ArmyOnly | FleetOnly | BothUnits | ConvoyOnly deriving (Eq, Ord)

instance Show RouteType where
  show ArmyOnly = "[A]"
  show FleetOnly = "[F]"
  show BothUnits = "[B]"
  show ConvoyOnly = "[C]"

data Route = Route { space1 :: Space, space2 :: Space, routeType :: RouteType }

instance Show Route where
  show r = let spn1 = spaceName (space1 r) 
               spn2 = spaceName (space2 r) 
           in min spn1 spn2 ++ "-" ++ max spn1 spn2 ++ " " ++ show (routeType r)

instance Eq Route where
  (==) route1 route2 = 
    let max1 = max (space1 route1) (space2 route1)
        max2 = max (space1 route2) (space2 route2)
        min1 = min (space1 route1) (space2 route1)
        min2 = min (space1 route2) (space2 route2)
    in max1 == max2 && min1 == min2

-- order does not depend on which component has which space
instance Ord Route where
  (<=) route1 route2 = 
    let max1 = max (space1 route1) (space2 route1)
        max2 = max (space1 route2) (space2 route2)
        min1 = min (space1 route1) (space2 route1)
        min2 = min (space1 route2) (space2 route2)
        type1 = routeType route1 
        type2 = routeType route2
    in min1 < min2 || (min1 == min2 && max1 <= max2)

routeWithoutDuplicates :: Route -> Validated Route
routeWithoutDuplicates r = case space1 r == space2 r of
  True  -> ValidationError $ "A route cannot lead to itself: " ++ show r
  False -> Valid r

-- A specific type for convoy path
data ConvoyPath = ConvoyPath { cpVia :: NonEmpty Space
                             , cpTo :: Space } deriving (Eq, Ord)

showMid :: NonEmpty Space -> String
showMid (s1 :| s2) = intercalate " " . fmap spaceName $ s1:s2

instance Show ConvoyPath where
  show (ConvoyPath via to) = "via " ++ showMid via ++ " to " ++ spaceName to

viaWithoutDuplicates cp = safeToSet (NE.toList . cpVia $ cp) *> return cp

endPointWithoutDuplicates spcFrom cp = case spcFrom == cpTo cp of
  True  -> ValidationError $ "The convoy path cannot start from and end to the same space."
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
  show (Area n memb False) = n ++ ": " ++ (intercalate "~" . fmap spaceName . S.toAscList $ memb)

simpleAreaFromSpace :: Space -> Area
simpleAreaFromSpace spc = Area (spaceName spc) (S.singleton spc) True

spaceInArea :: Space -> Area -> Bool
spaceInArea s (Area _ sa _) = S.member s sa

