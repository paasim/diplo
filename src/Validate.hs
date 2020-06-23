{-# LANGUAGE NoImplicitPrelude #-}
module Validate
  ( validateOrders
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

validateOrderUniqueness :: Set (Unit, Space) -> [Order] -> Validated [Order]
validateOrderUniqueness _ [] = Valid []
validateOrderUniqueness orderedUnits (o:os) = 
  let (u, s)      = getOrderedUnit o
      validatedOs = validateOrderUniqueness (S.insert (u, s) orderedUnits) os
  in case S.member (u, s) orderedUnits of
       False -> (:) o <$> validatedOs
       True  -> ValidationError $ "Multiple orders for '" ++ show u ++ "', '" ++ show s ++ "'."

validateOrders :: [Order] -> Validated [Order]
validateOrders orders = validateOrderUniqueness S.empty orders

