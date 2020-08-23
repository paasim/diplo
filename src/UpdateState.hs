{-# LANGUAGE NoImplicitPrelude #-}
module UpdateState 
  ( applyMovements
  , applyRetreats
  , applyBuilds
  ) where

import Error
import Unit
import Util
import Province
import Order
import Board
import BState
import RIO
import Data.Bifunctor ( bimap )
import qualified RIO.List as L
import qualified RIO.Map as M
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as S


-- executing orders, ie. updating state
-- occupiers
setOccupiers :: (Map Province Unit -> Map Province Unit) -> BState -> BState
setOccupiers f (BState gb gp con occ dus) = BState gb gp con (f occ) dus

deleteOccupier :: Province -> BState -> BState
deleteOccupier prov = setOccupiers (M.delete prov)

insertOccupier :: Province -> Unit -> BState -> BState
insertOccupier prov u = setOccupiers (M.insert prov u)

-- dislodged units
setDislodgedUnits :: (Set DislodgedUnit -> Set DislodgedUnit) -> BState -> BState
setDislodgedUnits f (BState gb gp con occ dus) = BState gb gp con occ (f dus)

insertDislodgedUnit :: Unit -> Province -> Province -> BState -> BState
insertDislodgedUnit unit at from = 
  setDislodgedUnits (S.insert (DislodgedUnit unit at from))

deleteDislodgedUnit :: Province -> BState -> BState 
deleteDislodgedUnit prov = setDislodgedUnits (S.filter ((/=) prov . dislodgedAt))

-- controllers
addControllers :: Board -> Map Province Unit -> Map Province Country -> Map Province Country
addControllers board msu mac =
    foldr (uncurry M.insert) mac -- insert/update new controllers
  . ungroupFst
  . fmap (bimap (NE.toList . areaProvinces . toArea board) unitCountry) -- (prov,unit) from ([prov],country)
  . filter (\(prov, unit) -> M.member prov (boardSupplyCenters board))
  . M.toList $ msu

occupiersToControllers :: BState -> BState
occupiersToControllers (BState gb gp con occ dus) =
  BState gb gp (addControllers gb occ con) occ dus

recalculateControl :: BState -> Validated BState
recalculateControl state = case season . gamePhase $ state of
  Spring -> Valid state
  Fall   -> Valid (occupiersToControllers state)
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
dislodgeUnit :: Province -> Province -> BState -> BState
dislodgeUnit from at state = case M.lookup at (occupiers state) of
  Just u  -> insertDislodgedUnit u at from . deleteOccupier at $ state 
  Nothing -> state

applyDislodge :: Order -> BState -> BState
applyDislodge (Order u from (Attack to))          = dislodgeUnit from to
applyDislodge (Order u from (AttackViaConvoy cp)) = dislodgeUnit from (cpTo cp)
applyDislodge _                                   = id

applyDelete :: Order -> BState -> BState
applyDelete (Order _ from (Attack _))          = deleteOccupier from
applyDelete (Order _ from (AttackViaConvoy _)) = deleteOccupier from
applyDelete _                                  = id

applyInsert :: Order -> BState -> BState
applyInsert (Order u _ (Attack to))          = insertOccupier to u
applyInsert (Order u _ (AttackViaConvoy cp)) = insertOccupier (cpTo cp) u
applyInsert _                                = id


-- also go to next phase if no dislodged units (ie. retreat phase skipped)
applyMovements :: BState -> [Order] -> Validated BState
applyMovements state orders = nextPhaseIfNoDislodged
  -- then insert the units to the location they are moving to
  . (flip <$> foldr) applyInsert orders
  -- then dislodge the units from the location they are moving to
  . (flip <$> foldr) applyDislodge orders
  -- first delete units from the location they are moving from
  . (flip <$> foldr) applyDelete orders
  $ state


-- retreat phase
applyRetreat :: RetreatOrder -> BState -> BState
applyRetreat (RODisband _ prov)           = deleteDislodgedUnit prov
applyRetreat (RORetreat u provFrom provTo) =
  deleteDislodgedUnit provFrom . insertOccupier provTo u

-- recalculate controlled areas
applyRetreats :: BState -> [RetreatOrder] -> Validated BState
applyRetreats state = fmap nextPhase . recalculateControl . foldr applyRetreat state


-- build phase
applyBuild :: BuildOrder -> BState -> BState
applyBuild (BODisband _ prov)  = deleteOccupier prov
applyBuild (BOBuild c prov ut) = insertOccupier prov (Unit c ut)

-- this wouldn't need Validated 
-- but better to make it consistent with applyRetreats and applyMovements
applyBuilds :: BState -> [BuildOrder] -> Validated BState
applyBuilds state = Valid . nextPhase . foldr applyBuild state

