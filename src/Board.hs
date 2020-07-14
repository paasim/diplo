{-# LANGUAGE NoImplicitPrelude #-}
module Board
  ( Board (..)
  , mkBoard
  , SupplyOrigin (..), supplyOriginToCountry
  , findArea
  ) where

import Error
import Unit
import Util
import Space
import Order
import RIO
import qualified RIO.Set as S
import qualified RIO.Map as M

-- this is for indicating which supply centers can be used for 
data SupplyOrigin = Common | HomeSupply Country deriving (Eq, Ord)

supplyOriginToCountry :: SupplyOrigin -> Maybe Country
supplyOriginToCountry (HomeSupply c) = Just c
supplyOriginToCountry Common         = Nothing

instance Show SupplyOrigin where
  show Common = "Common"
  show (HomeSupply c) = show c

-- Definition of the boad and related functions
data Board = Board { boardSpaces :: Set Space
                   , boardRoutes :: Map Route RouteType
                   -- used to specify that an attack to a coast also attacks
                   -- to the corresponding land and vice versa
                   , boardAreas :: Set Area 
                   , boardSupplyCenters :: Map Area SupplyOrigin
                   }

-- prints space with supply info
printArea :: Board -> Area -> String
printArea board area = case M.lookup area (boardSupplyCenters board) of
  (Just supplyOrigin) -> show area <> " [SC, " <> show supplyOrigin <> "]"
  Nothing             -> show area

showPair :: (Show a, Show b) => (a, b) -> String
showPair (a, b) = show a <> " " <> show b

instance Show Board where
  show board = "Spaces:\n" <> unlines (fmap show . S.toAscList . boardSpaces $ board)
    <> "\nRoutes:\n" <> unlines (fmap showPair . M.toList . boardRoutes $ board)
    <> "\nAreas:\n" <> unlines (fmap (printArea board) . S.toAscList . boardAreas $ board)

-- add space if does not belong to multiple areas
-- maybe this could be separated into two parts, first check and then add
checkArea :: Space -> Set Area -> Validated (Set Area)
checkArea s sa = case length . S.filter (spaceInArea s) $ sa of
  1 -> Valid sa
  0 -> Valid $ S.insert (simpleAreaFromSpace s) sa
  _ -> ValidationError $ "Space '" <> show s <> "' belongs to multiple areas."

addSpacesAsAreas :: [Space] -> Set Area -> Validated (Set Area)
addSpacesAsAreas spcs sa = foldr (\spc -> (>>= checkArea spc)) (Valid sa) spcs

mkAreas :: [(Area, a)] -> Validated (Set Area)
mkAreas = safeToSet . fmap fst

spacesInAreas :: [Area] -> Set Space
spacesInAreas = foldr (S.union . areaMembers) S.empty

-- spaces that are SupplyCenters and do not belong to an area
spaceSCs :: [Area] -> [(Space, Maybe SupplyOrigin)] -> [(Space, SupplyOrigin)]
spaceSCs areas = filter (flip S.notMember (spacesInAreas areas) . fst) . filterJust

spaceToArea :: (Space, x) -> (Area, x)
spaceToArea (s, x) = (simpleAreaFromSpace s, x)

-- Areas with SupplyOrigin and Spaces with SupplyOrigin that dont belong to an area
mkSupplyCenters :: [(Space, Maybe SupplyOrigin)] -> [(Area, Maybe SupplyOrigin)] -> Validated (Map Area SupplyOrigin)
mkSupplyCenters ss as = safeToMap $
  filterJust as <> (fmap spaceToArea . spaceSCs (fmap fst as)) ss

-- this converts simlespaces to areas twice, onc in addSpacesAsAreas
-- and then once in mkSupplyCenters
mkBoard :: [(Space, Maybe SupplyOrigin)] -> [(Route, RouteType)] -> [(Area, Maybe SupplyOrigin)] -> Validated Board
mkBoard spaceList routeList areaList =
  Board <$> (safeToSet . fmap fst) spaceList
        <*> safeToMap routeList
        <*> ((safeToSet . fmap fst) areaList >>= addSpacesAsAreas (fmap fst spaceList))
        <*> mkSupplyCenters spaceList areaList

findArea :: Board -> Space -> Validated Area
findArea board spc = case getUnique (spaceInArea spc) . boardAreas $ board of
  Just a  -> Valid a
  Nothing -> ValidationError $ "Space " <> show spc
                              <> " does not belong to a unique area."

