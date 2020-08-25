{-# LANGUAGE NoImplicitPrelude #-}
module Validate 
  ( checkDuplicates
  , getExecutableOrders
  , getExecutableRetreatOrders
  , getExecutableBuildOrders
  ) where
  
import Error
import Unit
import Util
import Order
import Province
import Board
import BState
import Parse
import RIO
import qualified RIO.Map as M
import qualified RIO.Set as S


-- orders
checkDuplicates :: [Order] -> Validated [Order]
checkDuplicates = fmap S.toList . safeToSet

-- by default hold for all units
initialOrders :: BState -> Map Province Order
initialOrders = M.fromList
              . fmap (\x -> (fst x, uncurry Order (swap x) Hold))
              . M.toList . occupiers

insertValidOrder :: BState -> Map Province Order -> String -> Map Province Order
insertValidOrder state m orderString = case parseValidated (parseOrder state) orderString of
  Valid order -> M.insert (orderProvince order) order m
  _           -> m

-- map space order ensures that each unit has the newest order at the end
foldValidOrders :: BState -> [String] -> Map Province Order
foldValidOrders state = foldl' (insertValidOrder state) (initialOrders state)

getExecutableOrders :: BState -> [String] -> Validated [Order]
getExecutableOrders state = Valid . M.elems . foldValidOrders state


-- retreat orders
dislodgedUnitToDisbandOrder :: DislodgedUnit -> (Province, RetreatOrder)
dislodgedUnitToDisbandOrder = 
  (,) <$> dislodgedAt <*> (RODisband <$> dislodgedUnit <*> dislodgedAt)

-- by default disband every unit
initialRetreatOrders :: Set DislodgedUnit -> Map Province RetreatOrder
initialRetreatOrders =
  M.fromList . fmap dislodgedUnitToDisbandOrder . S.toList 

insertValidRO :: BState -> Map Province RetreatOrder -> String -> Map Province RetreatOrder
insertValidRO state m ro = case parseValidated (parseRetreatOrder state) ro of
    Valid retreat -> M.insert (retreatOrderAt retreat) retreat m
    _             -> m

-- map space RO ensures that each unit has the newest RO at the end
foldValidRetreatOrders :: BState -> [String] -> Map Province RetreatOrder
foldValidRetreatOrders state = foldl' (insertValidRO state) M.empty

-- find orders with duplicated retreat to -areas
nonDuplicateAreas :: Board -> [(Province, RetreatOrder, Maybe Area)] -> [(Province, RetreatOrder)]
nonDuplicateAreas board l =
  let dupl = S.filter (Nothing /=) . getDuplicates . fmap (\(a,b,c) -> c) $ l
  in fmap (\(s, ro, _) -> (s, ro)) . filter (\(s, ro, ma) -> S.notMember ma dupl) $ l

removeRetreatsToSameArea :: Board -> Map Province RetreatOrder -> Map Province RetreatOrder
removeRetreatsToSameArea board = M.fromList
                               . nonDuplicateAreas board
                               . fmap (\(p, ro) -> (p, ro, fmap (toArea board) . retreatOrderTo $ ro))
                               . M.toList

addToInitials :: BState -> Map Province RetreatOrder -> Map Province RetreatOrder
addToInitials state m = M.union m . initialRetreatOrders . dislodgedUnits $ state

-- remove orders that retreat to the same area after folding, which removes duplicates
getExecutableRetreatOrders :: BState -> [String] -> Validated [RetreatOrder]
getExecutableRetreatOrders state = Valid . M.elems
                                 . addToInitials state
                                 . removeRetreatsToSameArea (gameBoard state)
                                 . foldValidRetreatOrders state

-- build orders
takeNOccupiedProvince :: Country -> Int -> BState -> [(Country, Province)]
takeNOccupiedProvince c i = fmap ((,) c) . take i . fmap fst
                        . M.toList . M.filter ((==) c . unitCountry) . occupiers

-- initially disband units if one must
initialBuildOrders :: BState -> [(Country, BuildOrder)]
initialBuildOrders state = fmap (\(c,prov) -> (c, BODisband c prov)) . join
                         . fmap (\(c,i) -> takeNOccupiedProvince c (-i) state)
                         . filter ((>) 0 . snd) 
                         . M.toList . unitDifference $ state

insertValidBuildOrder :: BState -> String -> [(Country, BuildOrder)] -> [(Country, BuildOrder)]
insertValidBuildOrder state bo = case parseValidated (parseBuildOrder state) bo of
  (Valid bo) -> (:) (buildOrderCountry bo, bo)
  _          -> id

foldValidBuildOrders :: BState -> [String] -> [(Country, BuildOrder)]
foldValidBuildOrders state = foldr (insertValidBuildOrder state) []

-- take N first orders for each country, where the N is controlled-occupied
takeNFirst :: Map Country Int -> [(Country, BuildOrder)] -> Validated [(Country, BuildOrder)]
takeNFirst m []             = Valid []
takeNFirst m ((c, bo):rest) = case (fmap (compare 0) . M.lookup c $ m, bo) of
  (Just EQ, _)               -> takeNFirst m rest
  (Just LT, BOBuild _ _ _) -> (:) (c, bo) <$> takeNFirst (M.adjust (+ (-1)) c m) rest
  (Just GT, BODisband _ _) -> (:) (c, bo) <$> takeNFirst (M.adjust (+ 1) c m) rest
  (Nothing, _)               -> ValidationError "Build order for country that has equal amount of supply centers and units"
  _                          -> ValidationError "Invalid build order type"


getExecutableBuildOrders :: BState -> [String] -> Validated [BuildOrder]
getExecutableBuildOrders state =
    fmap (fmap snd)
  . takeNFirst (M.filter (/= 0) . unitDifference $ state)
  -- add all valid parsed orders after initial build orders
  -- and then take n first starting from the end of the list
  . reverse
  . (initialBuildOrders state <>)
  . foldValidBuildOrders state

