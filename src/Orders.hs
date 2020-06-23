{-# LANGUAGE NoImplicitPrelude #-}
module Orders
  ( Order (..)
  , OrderData (..)
  , getOrderedUnit
  ) where

import Spaces
import Units
import RIO

data OrderData = AutoHold  --this will be the result of a valid but failed order
               | Hold
               | Attack Space
               | SuppHold Unit Space
               | SuppAttack Unit Space Space
               | Convoy Unit Space Space
               | AttackViaConvoy ConvoyPath deriving Eq


instance Show OrderData where
  show AutoHold = "autoholds"
  show Hold = "holds"
  show (Attack spc) = "to " ++ spaceName spc
  show (SuppHold u spc) = "supports (" ++ show (Order u spc Hold) ++ ")"
  show (SuppAttack u spc1 spc2) = "supports (" ++ show (Order u spc1 (Attack spc2)) ++ ")"
  show (Convoy u1 spc1 spc2) = "convoys " ++ show u1 ++ " from " ++ spaceName spc1 ++ " to " ++ spaceName spc2
  show (AttackViaConvoy cp) = show cp

data Order = Order { orderUnit :: Unit
                   , orderSpace :: Space
                   , orderData :: OrderData } deriving Eq

instance Show Order where
  show (Order u spc od) = show u ++ " " ++ spaceName spc ++ " " ++ show od

getOrderedUnit :: Order -> (Unit, Space)
getOrderedUnit = (,) <$> orderUnit <*> orderSpace

