{-# LANGUAGE NoImplicitPrelude #-}
module BoardDefs
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


-- Definition of the boad and related functions
data Board = Board { boardSpaces :: Set Space
                   , boardRoutes :: Set Route
                   -- used to specify that an attack to a coast also attacks
                   -- to the corresponding land and vice versa
                   , boardAreas :: Set Area 
                   , boardSupplyCenters :: Set Area }

-- prints space with supply info
printArea :: Board -> Area -> String
printArea board area = show area ++ if S.member area (boardSupplyCenters board) then " [SC]" else ""

instance Show Board where
  show b = "Spaces:\n" ++ intercalate "\n" (fmap show . S.toAscList . boardSpaces $ b)
    ++ "\n\nRoutes:\n" ++ intercalate "\n" (fmap show . S.toAscList . boardRoutes $ b)
    ++ "\n\nAreas:\n"  ++ intercalate "\n" (fmap (printArea b) . S.toAscList . boardAreas $ b)

mkSpaces :: [Space] -> Validated (Set Space)
mkSpaces = safeToSet

mkRoutes :: [(Space, Space, RouteType)] -> Validated (Set Route)
mkRoutes [] = Valid S.empty
mkRoutes ((s1,s2,rt):rs) = do
  r <- routeWithoutDuplicates $ Route s1 s2 rt
  rs <- mkRoutes rs
  setInsertNonExisting r rs

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
      newSpaces = fmap fst . filter (\(s,b) -> b && not (S.member s spacesInAreas)) $ ss 
  in safeToSet ((fmap fst . filter snd) as ++ fmap simpleAreaFromSpace newSpaces)
  
mkBoard :: [(Space, Bool)] -> [(Space, Space, RouteType)] -> [(Area, Bool)] -> Validated Board
mkBoard spaceList routeData areaData = do
  spaces <- mkSpaces . fmap fst $ spaceList
  routes <- mkRoutes routeData
  areas <- mkAreas areaData >>= allSpacesHaveUniqueArea spaceList
  supplyCenters <- mkSupplyCenters spaceList areaData
  Valid $ Board spaces routes areas supplyCenters  


getSpacesFromArea :: Board -> Space -> [Space]
getSpacesFromArea b s = foldr ((++) . getSpacesFromArea) [] . S.filter (spaceInArea s) $ boardAreas b where
  getSpacesFromArea (Area _ members _) = S.toAscList members
