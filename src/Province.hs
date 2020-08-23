{-# LANGUAGE NoImplicitPrelude #-}
module Province
  ( Province (..)
  , ProvinceType (..)
  , Route (..)
  , RouteType (..)
  , ConvoyPath (..)
  ) where

import Error
import Util
import RIO
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as S

-- Provinces and related functions
data ProvinceType = Land | Ocean | Coast deriving (Eq, Ord, Show)

data Province = Province { provinceName :: String, provinceType :: ProvinceType }

instance Show Province where
  show s = provinceName s <> ", " <> (show . provinceType) s

instance Eq Province where
  (==) s1 s2 = provinceName s1 == provinceName s2

instance Ord Province where
  (<=) s1 s2 = provinceName s1 <= provinceName s2


-- neighbors aka "routes" are represented as unordered pairs
-- e.g. (a,b) is equivalent to (b, a)
data Route = Route { province1 :: Province, province2 :: Province }

instance Show Route where
  show r = let spn1 = provinceName (province1 r) 
               spn2 = provinceName (province2 r) 
           in min spn1 spn2 <> "-" <> max spn1 spn2-- <> " " <> show (routeType r)


maxProvince :: Route -> Province
maxProvince = max <$> province1 <*> province2
minProvince = min <$> province1 <*> province2

instance Eq Route where
  (==) route1 route2 = maxProvince route1 == maxProvince route2
                    && minProvince route1 == minProvince route2

-- order does not depend on which component has which province
instance Ord Route where
  (<=) route1 route2 = minProvince route1 < minProvince route2
    || (minProvince route1 == minProvince route2 && maxProvince route1 <= maxProvince route2)


-- this is mainly needed for convoy paths
data RouteType = ArmyOnly | FleetOnly | BothUnits | ConvoyOnly deriving (Eq, Ord)

instance Show RouteType where
  show ArmyOnly = "[A]"
  show FleetOnly = "[F]"
  show BothUnits = "[B]"
  show ConvoyOnly = "[C]"

-- A specific type for convoy path
data ConvoyPath = ConvoyPath { cpVia :: NonEmpty Province
                             , cpTo :: Province } deriving (Eq, Ord)

showMid :: NonEmpty Province -> String
showMid (s1 :| s2) = unwords . fmap provinceName $ s1:s2

instance Show ConvoyPath where
  show (ConvoyPath cpvia to) = "via " <> showMid cpvia <> " to " <> provinceName to

