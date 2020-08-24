{-# LANGUAGE NoImplicitPrelude #-}
module BState 
  ( BState (..)
  , mkBState
  , DislodgedUnit (..)
  , Phase (..), Season (..)
  , unitDifference
  , countriesInGame
  ) where

import Error
import Unit
import Util
import Province
import Order
import Board
import RIO
import Data.Bifunctor ( bimap )
import qualified RIO.List as L
import qualified RIO.Map as M
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as S

-- Game state and related functions
-- named BState to avoid confusion with the regular State type
data Season = Spring | Fall | Winter deriving (Eq, Show)
data Phase = Phase { season :: Season, year :: Int } deriving Eq

instance Show Phase where
  show (Phase season year) = show season <> " " <> show year

data DislodgedUnit = DislodgedUnit { dislodgedUnit :: Unit
                                   -- where the unit was
                                   , dislodgedAt :: Province   
                                   -- where the attack came from
                                   , dislodgedFrom :: Province
                                   } deriving (Eq, Ord)

instance Show DislodgedUnit where
  show du = show (dislodgedUnit du)
          <> " at " <> (provinceName . dislodgedAt) du 
          <> ", dislodged from " <> (provinceName . dislodgedFrom) du

data BState = BState { gameBoard :: Board
                     , gamePhase :: Phase
                     -- Countries control areas, ie. Stp
                     , controllers :: Map Area Country 
                     -- Units occupy provinces, ie StpSC
                     , occupiers :: Map Province Unit     
                     -- this is nonempty only after movement phase
                     , dislodgedUnits :: Set DislodgedUnit
                     }

-- errors if conflicting/duplicated information is given for
-- controllers, occuiers or dislodged units
mkBState :: Board -> Phase -> [(Province, Unit)] -> [(Area, Country)] -> [DislodgedUnit] -> Validated BState
mkBState board phase provinceStates areaStates dislodgedUnitList =
  BState board phase <$> safeToMap areaStates
                     <*> safeToMap provinceStates
                     <*> safeToSet dislodgedUnitList

-- These are for printing control and occupance information (sorted by country)
compareProvinces :: Ord b => (Province, b) -> (Province, b) -> Ordering
compareProvinces (prov1, b1) (prov2, b2) =
  case (compare prov1 prov2, compare b1 b2) of
    (comp, EQ) -> comp
    (_,  comp) -> comp

compareAreas :: Ord b => (Area, b) -> (Area, b) -> Ordering
compareAreas (a1, b1) (a2, b2) =
  case ((compare `on` NE.head . areaProvinces) a1 a2, compare b1 b2) of
    (comp, EQ) -> comp
    (_,  comp) -> comp

showStateFor :: Show b => (a -> String) -> String -> a -> b -> String
showStateFor showA label a b = showA a <> ", " <> label <> " by " <> show b

showProvinceStates :: BState -> [String]
showProvinceStates = fmap (uncurry (showStateFor provinceName "occupied")) 
                   . L.sortBy compareProvinces . M.toList . occupiers

showAreaStates :: BState -> [String]
showAreaStates = fmap (uncurry (showStateFor show "controlled")) 
               . L.sortBy compareAreas . M.toList . controllers

instance Show BState where
  show state = show (gamePhase state) <> ", status:\n"
    <> "\nProvinces:\n" <> unlines (showProvinceStates state)
    <> "\nAreas:\n"  <> unlines (showAreaStates state)
    <> "\nDislodged units:\n"
    <> unlines (fmap show . S.toList . dislodgedUnits $ state) <> "\n"

-- how many countries either control an area or occupy a province with a unit
countriesInGame :: BState -> [Country]
countriesInGame = L.union <$> getUniques . M.elems . controllers
                          <*> getUniques . M.elems . M.map unitCountry . occupiers

-- difference of between two different maps for a fiven key
countDifference :: Map Country Int -> Map Country Int -> Country -> Int
countDifference m1 m2 = (-) <$> fromMaybe 0 . flip M.lookup m1
                            <*> fromMaybe 0 . flip M.lookup m2

-- calculates the the difference for a given country given the state
controllersMOccupiers :: BState -> Country -> Int
controllersMOccupiers state =
  (countDifference <$> M.fromList . counts . M.elems . controllers
                   <*> M.fromList . counts . fmap unitCountry . M.elems . occupiers) state

-- difference in controlled areas and occupied units, needed for build/retreat phase
unitDifference :: BState -> Map Country Int
unitDifference state = M.fromList
                     . fmap ((,) <*> controllersMOccupiers state)
                     . countriesInGame $ state

