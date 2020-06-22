{-# LANGUAGE NoImplicitPrelude #-}
module Validate
  ( validateOrder
  , validateOrders
  ) where

import Board
import BState
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
validateOccupierType :: BState -> UnitType -> Space -> Validated Space
validateOccupierType state ut s = case fmap ((== ut) . unitType) . M.lookup s $ occupiers state of
  (Just True) -> Valid s
  _ -> ValidationError $ "Space '" ++ show s ++ "' is not occupied by a unit of type '" ++ show ut ++ "'."

validateOrder :: BState -> Order -> Validated Order
validateOrder state (Order oUnit oSpc oData) = case oData of
  (AttackViaConvoy cp) -> do
    toListChecker (validateOccupierType state Fleet) . NE.toList . cpVia $ cp
    cpWithoutDuplicates oSpc cp -- this should be done at the parsing stage
    return $ Order oUnit oSpc (AttackViaConvoy cp)
  validOdata           -> Valid $ Order oUnit oSpc validOdata

validateOrderUniqueness :: Set (Unit, Space) -> [Order] -> Validated [Order]
validateOrderUniqueness _ [] = Valid []
validateOrderUniqueness orderedUnits (o:os) = 
  let (u, s)      = getOrderedUnit o
      validatedOs = validateOrderUniqueness (S.insert (u, s) orderedUnits) os
  in case S.member (u, s) orderedUnits of
       False -> (:) o <$> validatedOs
       True  -> ValidationError $ "Multiple orders for '" ++ show u ++ "', '" ++ show s ++ "'."

validateOrders :: BState -> [Order] -> Validated [Order]
validateOrders state o = toListChecker (validateOrder state) o >>= validateOrderUniqueness S.empty

