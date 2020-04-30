{-# LANGUAGE NoImplicitPrelude #-}
module StateDefs 
  ( GameState (..)
  , mkGameState
  , Phase (..)
  , showGameBoard
  ) where

import BoardDefs
import Spaces
import Units
import Errors
import Utils
import RIO
import RIO.List ( intercalate )
import qualified RIO.Map as M ( lookup, member )
import qualified RIO.Set as S ( member, toAscList )

-- Game state and related functions
data Phase = Spring | Fall deriving (Eq, Show)
data GameState = GameState { gameBoard :: Board
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

mkGameState :: Board -> Int -> Phase -> [(Space, Maybe Unit)] -> [(Area, Maybe Country)] -> Validated GameState
mkGameState board year phase spaceStates areaStates = do 
 controllers <- safeToMap $ getControllerData areaStates
 occupiers <- safeToMap $ getOccupierData spaceStates
 Valid $ GameState board (year, phase) controllers occupiers

printStateFor :: (Show b) => (a -> String) -> String -> a -> Maybe b -> String
printStateFor showA str a Nothing  = showA a ++ ", un" ++ str
printStateFor showA str a (Just b) = showA a ++ ", " ++ str ++ " by " ++ show b

statesFor :: (Ord a, Show b) => (GameState -> Set a) -> (GameState -> Map a b) -> (a -> String) -> String -> GameState -> [String]
statesFor getKeys stateMap showKey showWith gs =
  fmap (printStateFor showKey showWith <*> (flip M.lookup) (stateMap gs))
  . filter ((flip M.member) (stateMap gs)) -- this skips all uncontrolled/unoccupied spaces/areas
  . S.toAscList . getKeys $ gs

areaStates  = statesFor (boardAreas  . gameBoard) controllers areaName "controlled"
spaceStates = statesFor (boardSpaces . gameBoard) occupiers  spaceName "occupied"

instance Show GameState where
  show gs = (show . fst . gamePhase) gs ++ " " ++ (show . snd. gamePhase) gs ++ ", status:\n\n"
          ++ "Spaces:\n"
          ++ intercalate "\n" (spaceStates gs) ++ "\n\n"
          ++ "Areas:\n"
          ++ intercalate "\n" (areaStates gs)

showGameBoard :: GameState -> String
showGameBoard = show . gameBoard

