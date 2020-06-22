{-# LANGUAGE NoImplicitPrelude #-}
module BState 
  ( BState (..)
  , mkBState
  , Phase (..)
  , showGameBoard
  ) where

import Board
import Spaces
import Units
import Errors
import Utils
import RIO
import RIO.List ( intercalate )
import qualified RIO.Map as M ( lookup, member )
import qualified RIO.Set as S ( member, toAscList )

-- Game state and related functions
-- named BState to avoid confusion with the regular State type
data Phase = Spring | Fall deriving (Eq, Show)
data BState = BState { gameBoard :: Board
                     , gamePhase :: (Int, Phase)
                     , controllers :: Map Area Country
                     , occupiers :: Map Space Unit }

getControllerData :: [(Area, Maybe Country)] -> [(Area, Country)]
getControllerData [] = []
getControllerData ((s, Nothing):rest) = getControllerData rest
getControllerData ((s, Just c):rest) = (s, c) : getControllerData rest

getOccupierData :: [(Space, Maybe Unit)] -> [(Space, Unit)]
getOccupierData [] = []
getOccupierData ((s, Nothing):rest) = getOccupierData rest
getOccupierData ((s, Just u):rest) = (s, u) : getOccupierData rest

mkBState :: Board -> Int -> Phase -> [(Space, Maybe Unit)] -> [(Area, Maybe Country)] -> Validated BState
mkBState board year phase spaceStates areaStates = do 
 controllers <- safeToMap $ getControllerData areaStates
 occupiers <- safeToMap $ getOccupierData spaceStates
 Valid $ BState board (year, phase) controllers occupiers

printStateFor :: (Show b) => (a -> String) -> String -> a -> Maybe b -> String
printStateFor showA str a Nothing  = showA a ++ ", un" ++ str
printStateFor showA str a (Just b) = showA a ++ ", " ++ str ++ " by " ++ show b

statesFor :: (Ord a, Show b) => (BState -> Set a) -> (BState -> Map a b) -> (a -> String) -> String -> BState -> [String]
statesFor getKeys stateMap showKey showWith state =
  fmap (printStateFor showKey showWith <*> flip M.lookup (stateMap state))
  . filter (flip M.member (stateMap state)) -- this skips all uncontrolled/unoccupied spaces/areas
  . S.toAscList . getKeys $ state

areaStates  = statesFor (boardAreas  . gameBoard) controllers areaName "controlled"
spaceStates = statesFor (boardSpaces . gameBoard) occupiers  spaceName "occupied"

instance Show BState where
  show state = (show . fst . gamePhase) state
      ++ " " ++ (show . snd. gamePhase) state
      ++ ", status:\n\n"
      ++ "Spaces:\n"
      ++ intercalate "\n" (spaceStates state) ++ "\n\n"
      ++ "Areas:\n"
      ++ intercalate "\n" (areaStates state)

showGameBoard :: BState -> String
showGameBoard = show . gameBoard

