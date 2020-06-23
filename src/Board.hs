{-# LANGUAGE NoImplicitPrelude #-}
module Board
  ( Board (..)
  , mkBoard
  , getSpacesFromArea
  ) where

import Spaces
import Units
import Errors
import Utils
import RIO
import RIO.List ( intercalate )
import qualified RIO.Set as S ( empty, filter, insert, member, toAscList, union )
import qualified RIO.Map as M ( toList )


-- Definition of the boad and related functions
data Board = Board { boardSpaces :: Set Space
                   , boardRoutes :: Map Route RouteType
                   -- used to specify that an attack to a coast also attacks
                   -- to the corresponding land and vice versa
                   , boardAreas :: Set Area 
                   , boardSupplyCenters :: Set Area }

-- prints space with supply info
printArea :: Board -> Area -> String
printArea board area = show area ++ if S.member area (boardSupplyCenters board) then " [SC]" else ""

showPair :: (Show a, Show b) => (a, b) -> String
showPair (a, b) = show a ++ " " ++ show b

instance Show Board where
  show board = "Spaces:\n" ++ intercalate "\n" (fmap show . S.toAscList . boardSpaces $ board)
    ++ "\n\nRoutes:\n" ++ intercalate "\n" (fmap showPair . M.toList . boardRoutes $ board)
    ++ "\n\nAreas:\n"  ++ intercalate "\n" (fmap (printArea board) . S.toAscList . boardAreas $ board)

mkSpaces :: [Space] -> Validated (Set Space)
mkSpaces = safeToSet

mkRoutes :: [(Route, RouteType)] -> Validated (Map Route RouteType)
mkRoutes = safeToMap

allSpacesHaveUniqueArea :: [(Space, Bool)] -> Set Area -> Validated (Set Area)
allSpacesHaveUniqueArea []     sa = Valid sa
allSpacesHaveUniqueArea ((s,_):ss) sa = case length . S.filter (spaceInArea s) $ sa of
  1 -> allSpacesHaveUniqueArea ss sa
  0 -> allSpacesHaveUniqueArea ss (S.insert (simpleAreaFromSpace s) sa)
  _ -> ValidationError $ "Space '" ++ show s ++ "' belongs to multiple areas."

mkAreas :: [(Area, Bool)] -> Validated (Set Area)
mkAreas = safeToSet . fmap fst

mkSupplyCenters :: [(Space, Bool)] -> [(Area, Bool)] -> Validated (Set Area)
mkSupplyCenters ss as =
  let spacesInAreas = foldr (S.union . areaMembers) S.empty . fmap fst $ as
      newSpaces = fmap fst . filter (\(s,board) -> board && not (S.member s spacesInAreas)) $ ss 
  in safeToSet ((fmap fst . filter snd) as ++ fmap simpleAreaFromSpace newSpaces)
  
mkBoard :: [(Space, Bool)] -> [(Route, RouteType)] -> [(Area, Bool)] -> Validated Board
mkBoard spaceList routeList areaData = do
  spaces <- mkSpaces . fmap fst $ spaceList
  routes <- mkRoutes routeList
  areas <- mkAreas areaData >>= allSpacesHaveUniqueArea spaceList
  supplyCenters <- mkSupplyCenters spaceList areaData
  Valid $ Board spaces routes areas supplyCenters  

getSpacesFromArea :: Board -> Space -> [Space]
getSpacesFromArea board s =
  concatMap getSpacesFromArea . S.filter (spaceInArea s) $ boardAreas board where
    getSpacesFromArea (Area _ members _) = S.toAscList members

