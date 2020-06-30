{-# LANGUAGE NoImplicitPrelude #-}
module BState 
  ( BState (..)
  , mkBState
  , DislodgedUnit (..)
  , Phase (..), Season (..)
  , unitDifference
  , countriesInGame
  , applyMovements
  , applyRetreats
  , applyBuilds
  ) where

import Error
import Unit
import Util
import Space
import Order
import Board
import RIO
import qualified RIO.List as L ( sortBy, union )
import qualified RIO.Map as M ( delete, elems, fromList, insert, lookup, map, member, toList )
import qualified RIO.Set as S ( filter, insert, null, toList )

-- Game state and related functions
-- named BState to avoid confusion with the regular State type
data Season = Spring | Fall | Winter deriving (Eq, Show)
data Phase = Phase { season :: Season, year :: Int } deriving Eq

instance Show Phase where
  show (Phase season year) = show season <> " " <> show year

data DislodgedUnit = DislodgedUnit { dislodgedUnit :: Unit
                                   -- where the unit was
                                   , dislodgedAt :: Space   
                                   -- where the attack came from
                                   , dislodgedFrom :: Space
                                   } deriving (Eq, Ord)

instance Show DislodgedUnit where
  show du = show (dislodgedUnit du)
          <> " at " <> (spaceName . dislodgedAt) du 
          <> ", dislodged from " <> (spaceName . dislodgedFrom) du

data BState = BState { gameBoard :: Board
                     , gamePhase :: Phase
                     -- Countries control areas, ie. Stp
                     , controllers :: Map Area Country 
                     -- Units occupy spaces, ie StpSC
                     , occupiers :: Map Space Unit     
                     -- this is nonempty only after movement phase
                     , dislodgedUnits :: Set DislodgedUnit
                     }

-- errors if conflicting/duplicated information is given for
-- controllers, occuiers or dislodged units
mkBState :: Board -> Phase -> [(Space, Maybe Unit)] -> [(Area, Maybe Country)] -> [DislodgedUnit] -> Validated BState
mkBState board phase spaceStates areaStates dislodgedUnitList =
  BState board phase <$> safeToMap (filterJust areaStates)
                     <*> safeToMap (filterJust spaceStates)
                     <*> safeToSet dislodgedUnitList

-- These are for printing control and occupance information (sorted by country)
compareOccupiers :: (Space, Unit) -> (Space, Unit) -> Ordering
compareOccupiers (s1, u1) (s2, u2) = case (compare s1 s2, compare u1 u2) of
  (comp, EQ) -> comp
  (_,  comp) -> comp

compareControllers :: (Area, Country) -> (Area, Country) -> Ordering
compareControllers (a1, c1) (a2, c2) = case (compare a1 a2, compare c1 c2) of
  (comp, EQ) -> comp
  (_,  comp) -> comp

showStateFor :: Show b => (a -> String) -> String -> a -> b -> String
showStateFor showA label a b = showA a <> ", " <> label <> " by " <> show b

showSpaceStates :: BState -> [String]
showSpaceStates = fmap (uncurry (showStateFor spaceName "occupied")) 
                . L.sortBy compareOccupiers . M.toList . occupiers

showAreaStates :: BState -> [String]
showAreaStates = fmap (uncurry (showStateFor areaName "controlled")) 
                . L.sortBy compareControllers . M.toList . controllers

instance Show BState where
  show state = show (gamePhase state) <> ", status:\n"
    <> "\nSpaces:\n" <> unlines (showSpaceStates state)
    <> "\nAreas:\n"  <> unlines (showAreaStates state)
    <> "\nDislodged units:\n"
    <> unlines (fmap show . S.toList . dislodgedUnits $ state)

-- how many countries either control an area or occupy a space with a unit
countriesInGame :: BState -> [Country]
countriesInGame = L.union <$> getUniques . M.elems . controllers
                          <*> getUniques . M.elems . M.map unitCountry . occupiers

-- difference of between two different maps for a fiven key
countDifference :: Map Country Int -> Map Country Int -> Country -> Int
countDifference m1 m2 = (-) <$> fromMaybe 0 . flip M.lookup m1
                            <*> fromMaybe 0 . flip M.lookup m2

-- calculates the the difference for a given country given the state
controllersMOccupiers :: BState -> Country -> Int
controllersMOccupiers =
  countDifference <$> M.fromList . counts . M.elems . controllers
                  <*> M.fromList . counts . fmap unitCountry . M.elems . occupiers

-- difference in controlled areas and occupied units, needed for build/retreat phase
unitDifference :: BState -> Map Country Int
unitDifference state = M.fromList
                     . fmap ((,) <*> controllersMOccupiers state)
                     . countriesInGame $ state


-- executing orders, ie. updating state
-- occupiers
setOccupiers :: (Map Space Unit -> Validated (Map Space Unit)) -> BState -> Validated BState
setOccupiers f (BState gb gp con occ dus) =
  (\occNew -> BState gb gp con occNew dus) <$> f occ

deleteOccupier :: Space -> BState -> Validated BState
deleteOccupier spc = setOccupiers (Valid . M.delete spc)

insertOccupier :: Space -> Unit -> BState -> Validated BState
insertOccupier spc u = setOccupiers (Valid . M.insert spc u)

-- dislodged units
setDislodgedUnits :: (Set DislodgedUnit -> Validated (Set DislodgedUnit)) -> BState -> Validated BState
setDislodgedUnits f (BState gb gp con occ dus) = BState gb gp con occ <$> f dus

insertDislodgedUnit :: Unit -> Space -> Space -> BState -> Validated BState
insertDislodgedUnit unit at from = 
  setDislodgedUnits (Valid . S.insert (DislodgedUnit unit at from))

deleteDislodgedUnit :: Space -> BState -> Validated BState 
deleteDislodgedUnit spc = 
  setDislodgedUnits (Valid . S.filter ((/=) spc . dislodgedAt))

-- controllers
addArea :: Board -> (Space, Unit) -> Validated [(Area, Country)] -> Validated [(Area, Country)]
addArea board (spc, u) = case findArea board spc of
  (Valid a)           -> fmap ((a, unitCountry u) :)
  (ValidationError e) -> const (ValidationError e)
  (ParsingError e)    -> const (ParsingError e)

filterAreasWithSCs :: Board -> [(Area, Country)] -> [(Area, Country)]
filterAreasWithSCs board = filter (\(a,_) -> M.member a (boardSupplyCenters board))

addControllers :: Board -> Map Space Unit -> Map Area Country -> Validated (Map Area Country)
addControllers board msu mac =
    fmap (foldr (uncurry M.insert) mac) -- insert/update new controllers
  . fmap (filterAreasWithSCs board)     -- keep only supplycenters
  . foldr (addArea board) (Valid [])    -- (spc,unit) from (area,country)
  . M.toList $ msu

occupiersToControllers :: BState -> Validated BState
occupiersToControllers (BState gb gp con occ dus) =
  (\conNew -> BState gb gp conNew occ dus) <$> addControllers gb occ con

recalculateControl :: BState -> Validated BState
recalculateControl state = case season . gamePhase $ state of
  Spring -> Valid state
  Fall   -> occupiersToControllers state
  Winter -> ValidationError "Retreat phase cannot happen in Winter"

-- phase
incrPhase :: Phase -> Phase
incrPhase (Phase Spring i) = Phase Fall i
incrPhase (Phase Fall i)   = Phase Winter i
incrPhase (Phase Winter i) = Phase Spring (i+1)

nextPhase :: BState -> BState
nextPhase (BState gb gp con occ dus) = BState gb (incrPhase gp) con occ dus

-- also recalculate control (if it is fall)
nextPhaseIfNoDislodged :: BState -> Validated BState
nextPhaseIfNoDislodged state = case S.null . dislodgedUnits $ state of
  True  -> fmap nextPhase . recalculateControl $ state
  False -> Valid state


-- movement phase
dislodgeUnit :: Space -> Space -> BState -> Validated BState
dislodgeUnit from at state = case M.lookup at (occupiers state) of
  Just u  -> deleteOccupier at state >>= insertDislodgedUnit u at from
  Nothing -> Valid state

applyDislodge :: Order -> BState -> Validated BState
applyDislodge (Order u from (Attack to))          = dislodgeUnit from to
applyDislodge (Order u from (AttackViaConvoy cp)) = dislodgeUnit from (cpTo cp)
applyDislodge _                                   = Valid

applyDelete :: Order -> BState -> Validated BState
applyDelete (Order _ from (Attack _))          = deleteOccupier from
applyDelete (Order _ from (AttackViaConvoy _)) = deleteOccupier from
applyDelete _                                  = Valid

applyInsert :: Order -> BState -> Validated BState
applyInsert (Order u _ (Attack to))          = insertOccupier to u
applyInsert (Order u _ (AttackViaConvoy cp)) = insertOccupier (cpTo cp) u
applyInsert _                                = Valid


-- also go to next phase if no dislodged units (ie. retreat phase skipped)
applyMovements :: BState -> [Order] -> Validated BState
applyMovements state orders = (>>= nextPhaseIfNoDislodged)
  -- then insert the units to the location they are moving to
  . (flip <$> foldr) (\o -> (>>= applyInsert o)) orders
  -- then dislodge the units from the location they are moving to
  . (flip <$> foldr) (\o -> (>>= applyDislodge o)) orders
  -- first delete units from the location they are moving from
  . (flip <$> foldr) (\o -> (>>= applyDelete o)) orders
  . Valid $ state


-- retreat phase
applyRetreat :: RetreatOrder -> BState -> Validated BState
applyRetreat (RODisband _ spc)           = deleteDislodgedUnit spc
applyRetreat (RORetreat u spcFrom spcTo) = insertOccupier spcTo u
                                        >=> deleteDislodgedUnit spcFrom

-- recalculate controlled areas
applyRetreats :: BState -> [RetreatOrder] -> Validated BState
applyRetreats state = foldr (\ro vState -> vState >>= applyRetreat ro) (Valid state)
                   >=> recalculateControl
                   >=> (return . nextPhase)


-- build phase
applyBuild :: BuildOrder -> BState -> Validated BState
applyBuild (BODisband _ spc)  = deleteOccupier spc
applyBuild (BOBuild c spc ut) = insertOccupier spc (Unit c ut)

applyBuilds :: BState -> [BuildOrder] -> Validated BState
applyBuilds state = fmap nextPhase . foldr (\ro -> (>>= applyBuild ro)) (Valid state)

