{-# LANGUAGE NoImplicitPrelude #-}
module Board
  ( Board (..)
  , mkBoard
  , SupplyOrigin (..), supplyOriginToCountry
  , Area (..), toArea
  ) where

import Error
import Unit
import Util
import Province
import Order
import RIO
import qualified RIO.List as L
import qualified RIO.Map as M
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as S

-- this is for indicating which supply centers can be used for 
data SupplyOrigin = Common | HomeSupply Country deriving (Eq, Ord)

supplyOriginToCountry :: SupplyOrigin -> Maybe Country
supplyOriginToCountry (HomeSupply c) = Just c
supplyOriginToCountry Common         = Nothing

instance Show SupplyOrigin where
  show Common = "Common"
  show (HomeSupply c) = show c

-- Area is a concept for certain provinces that cannot be occupied simultaneously
-- ie. SC and NC of Spa and Stp and EC and SC of Bul
data Area = Area { areaName :: String
                 , areaProvinces :: NonEmpty Province } deriving (Eq, Ord)

instance Show Area where
  show = areaName

showAreaProvinces :: Area -> String
showAreaProvinces = L.intercalate "~" . fmap provinceName . NE.toList . areaProvinces

showAreaWithProvinces :: Area -> String
showAreaWithProvinces a = show a <> ": " <> showAreaProvinces a

-- Definition of the boad and related functions
data Board = Board { boardProvinces :: Set Province
                   , boardRoutes :: Map Route RouteType
                   -- used to specify that an attack to a coast also attacks
                   -- to the corresponding land and vice versa
                   , boardAreas :: Map Province Area
                   , boardSupplyCenters :: Map Province SupplyOrigin
                   }

instance Show Board where
  show board = "Provinces:\n" <> unlines (fmap show . S.toList . boardProvinces $ board)
    <> "\nRoutes:\n" <> unlines (fmap showPair . M.toList . boardRoutes $ board)
    <> "\nAreas:\n" <> unlines (fmap showAreaWithProvinces . L.nub . M.elems . boardAreas $ board)

mkSupplyCenters :: [(Province, Maybe SupplyOrigin)] -> Validated (Map Province SupplyOrigin)
mkSupplyCenters = safeToMap . filterJust

mkBoardAreas :: [Area] -> Map Province Area
mkBoardAreas = M.fromList . fmap swap . ungroupSnd . fmap ((,) <*> NE.toList . areaProvinces)

toArea :: Board -> Province -> Area
toArea board prov = case M.lookup prov (boardAreas board) of
  (Just area) -> area
  Nothing     -> Area (provinceName prov) (prov :| [])

mkBoard :: [(Province, Maybe SupplyOrigin)] -> [(Route, RouteType)] -> [Area] -> Validated Board
mkBoard provinceList routeList areaList =
  Board <$> (safeToSet . fmap fst) provinceList
        <*> safeToMap routeList
        <*> (return . mkBoardAreas) areaList
        <*> mkSupplyCenters provinceList

