{-# LANGUAGE NoImplicitPrelude #-}
module Validate
  ( validateOrder
  , validateOrders
  ) where

import BoardDefs
import StateDefs
import Spaces
import Units
import Orders
import Errors
import Utils
import RIO
import qualified RIO.NonEmpty as NE ( toList )
import qualified RIO.Map as M ( lookup )
import qualified RIO.Set as S ( empty, insert, member )

-- Validation after parsing
validateOccupier :: GameState -> Unit -> Space -> Validated Space
validateOccupier gs u s = case M.lookup s (occupiers gs) == Just u of
  True  -> Valid s
  False -> ValidationError $ "Unit '" ++ show u ++ "' does not occupy '" ++ show s ++ "'."

validateOccupierType :: GameState -> UnitType -> Space -> Validated Space
validateOccupierType gs ut s = case fmap ((== ut) . unitType) . M.lookup s $ (occupiers gs) of
  (Just True) -> Valid s
  _ -> ValidationError $ "Space '" ++ show s ++ "' is not occupied by a unit of type '" ++ show ut ++ "'."

validateOrderData :: GameState -> Unit -> Space -> OrderData -> Validated OrderData
validateOrderData gs u s Hold = Valid $ Hold
validateOrderData gs u s (Attack s2) = Valid $ Attack s2
validateOrderData gs u s (SuppHold u2 s2) = SuppHold u2 <$> validateOccupier gs u2 s2
validateOrderData gs u s (SuppAttack u2 s2 s3) = SuppAttack u2 <$> validateOccupier gs u2 s2 <*> pure s3
validateOrderData gs u s (Convoy u2 s2 s3) = Convoy u2 <$> validateOccupier gs u2 s2 <*> pure s3
validateOrderData gs u s (AttackViaConvoy cp) = do
  toListChecker (validateOccupierType gs Fleet) . NE.toList . cpVia $ cp
  AttackViaConvoy <$> cpWithoutDuplicates s cp -- this should be done at the parsing stage

validateOrder :: GameState -> Order -> Validated Order
validateOrder gs (Order oUnit oSpc oData) =
  Order oUnit <$> validateOccupier gs oUnit oSpc <*> validateOrderData gs oUnit oSpc oData

validateOrderUniqueness :: Set (Unit, Space) -> [Order] -> Validated [Order]
validateOrderUniqueness _ [] = Valid []
validateOrderUniqueness orderedUnits (o:os) = 
  let (u, s)      = getOrderedUnit o
      validatedOs = validateOrderUniqueness (S.insert (u, s) orderedUnits) os
  in case S.member (u, s) orderedUnits of
       False -> (:) o <$> validatedOs
       True  -> ValidationError $ "Multiple orders for '" ++ show u ++ "', '" ++ show s ++ "'."

validateOrders :: GameState -> [Order] -> Validated [Order]
validateOrders gs o = toListChecker (validateOrder gs) o >>= validateOrderUniqueness S.empty

