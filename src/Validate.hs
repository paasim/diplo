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
import Space
import Order
import Board
import BState
import Parse
import RIO
import qualified RIO.Map as M
import qualified RIO.Set as S


-- orders
checkDuplicates :: [Order] -> Validated [Order]
checkDuplicates = fmap S.toList . safeToSet

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

-- by default hold for all units
initialOrders :: BState -> Map Space Order
initialOrders = M.fromList
              . fmap (\x -> (fst x, uncurry Order (swap x) Hold))
              . M.toList . occupiers

insertValidOrder :: BState -> Map Space Order -> String -> Map Space Order
insertValidOrder state m orderString = case parseValidated (parseOrder state) orderString of
  Valid order -> M.insert (orderSpace order) order m
  _           -> m

-- map space order ensures that each unit has the newest order at the end
foldValidOrders :: BState -> [String] -> Map Space Order
foldValidOrders state = foldl' (insertValidOrder state) (initialOrders state)

getExecutableOrders :: BState -> [String] -> Validated [Order]
getExecutableOrders state = Valid . M.elems . foldValidOrders state


-- retreat orders
dislodgedUnitToDisbandOrder :: DislodgedUnit -> (Space, RetreatOrder)
dislodgedUnitToDisbandOrder = 
  (,) <$> dislodgedAt <*> (RODisband <$> dislodgedUnit <*> dislodgedAt)

-- by default disband every unit
initialRetreatOrders :: Set DislodgedUnit -> Map Space RetreatOrder
initialRetreatOrders =
  M.fromList . fmap dislodgedUnitToDisbandOrder . S.toList 

insertValidRO :: BState -> Map Space RetreatOrder -> String -> Map Space RetreatOrder
insertValidRO state m ro = case parseValidated (parseRetreatOrder state) ro of
    Valid retreat -> M.insert (retreatOrderAt retreat) retreat m
    _             -> m

-- map space RO ensures that each unit has the newest RO at the end
foldValidRetreatOrders :: BState -> [String] -> Map Space RetreatOrder
foldValidRetreatOrders state = foldl' (insertValidRO state) M.empty

commuteValidMaybe :: Maybe (Validated a) -> Validated (Maybe a)
commuteValidMaybe (Just (Valid a))           = Valid (Just a)
commuteValidMaybe Nothing                    = Valid Nothing 
commuteValidMaybe (Just (ValidationError e)) = ValidationError e
commuteValidMaybe (Just (ParsingError e))    = ParsingError e

-- find area for a retreat order, Nothing for a Disband order
areaForRetreatOrder :: Board -> RetreatOrder -> Validated (Maybe Area)
areaForRetreatOrder board = commuteValidMaybe . fmap (findArea board) . retreatOrderTo

addIfAreaValid :: Board -> (Space, RetreatOrder) -> Validated [(Space, RetreatOrder, Maybe Area)] -> Validated [(Space, RetreatOrder, Maybe Area)]
addIfAreaValid board (spc, ro) = case areaForRetreatOrder board ro of
  (Valid ma)          -> fmap ((spc, ro, ma) :)
  (ValidationError e) -> const (ValidationError e)
  (ParsingError e)    -> const (ParsingError e)

-- keep orders for which retreat to refers to a space that belongs to an area
validateRetreatToAreas :: Board -> [(Space, RetreatOrder)] -> Validated [(Space, RetreatOrder, Maybe Area)]
validateRetreatToAreas board = foldr (addIfAreaValid board) (Valid [])

-- find orders with duplicated retreat to -areas
findDuplicateAreas :: [(Space, RetreatOrder, Maybe Area)] -> Set (Maybe Area)
findDuplicateAreas = S.filter (/= Nothing) . getDuplicates . fmap (\(a,b,c) -> c)

nonDuplicateAreas :: Board -> [(Space, RetreatOrder, Maybe Area)] -> [(Space, RetreatOrder)]
nonDuplicateAreas board l = let dupl = findDuplicateAreas l
  in fmap (\(s, ro, _) -> (s, ro)) . filter (\(s, ro, ma) -> S.member ma dupl) $ l

removeRetreatsToSameArea :: Board -> Map Space RetreatOrder -> Validated (Map Space RetreatOrder)
removeRetreatsToSameArea board = fmap (M.fromList . nonDuplicateAreas board)
                               . validateRetreatToAreas board . M.toList

addToInitials :: BState -> Map Space RetreatOrder -> Map Space RetreatOrder
addToInitials state m = M.union m . initialRetreatOrders . dislodgedUnits $ state

-- remove orders that retreat to the same area after folding, which removes duplicates
getExecutableRetreatOrders :: BState -> [String] -> Validated [RetreatOrder]
getExecutableRetreatOrders state = fmap (M.elems . addToInitials state)
                                 . removeRetreatsToSameArea (gameBoard state)
                                 . foldValidRetreatOrders state


-- build orders
takeNOccupiedSpaces :: Country -> Int -> BState -> [(Country, Space)]
takeNOccupiedSpaces c i = fmap ((,) c) . take i . fmap fst
                        . M.toList . M.filter ((==) c . unitCountry) . occupiers

-- initially disband units if one must
initialBuildOrders :: BState -> [(Country, BuildOrder)]
initialBuildOrders state = fmap (\(c,spc) -> (c, BODisband c spc)) . join
                         . fmap (\(c,i) -> takeNOccupiedSpaces c (-i) state)
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

