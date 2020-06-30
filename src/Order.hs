{-# LANGUAGE NoImplicitPrelude #-}
module Order
  ( Order (..)
  , OrderData (..)
  , RetreatOrder (..) , retreatOrderAt, retreatOrderTo
  , BuildOrder (..), buildOrderCountry
  ) where

import Unit
import Space
import RIO

-- order referes to a regular movement phase order
data OrderData = Hold
               | Attack Space
               | SuppHold Unit Space
               | SuppAttack Unit Space Space
               | Convoy Unit Space Space
               | AttackViaConvoy ConvoyPath deriving (Eq, Ord)


instance Show OrderData where
  show Hold = "holds"
  show (Attack spc) = "to " <> spaceName spc
  show (SuppHold u spc) = "supports (" <> show (Order u spc Hold) <> ")"
  show (SuppAttack u spc1 spc2) = "supports (" <> show (Order u spc1 (Attack spc2)) <> ")"
  show (Convoy u1 spc1 spc2) = "convoys " <> show u1 <> " from " <> spaceName spc1 <> " to " <> spaceName spc2
  show (AttackViaConvoy cp) = show cp

data Order = Order { orderUnit :: Unit
                   , orderSpace :: Space
                   , orderData :: OrderData } deriving (Eq, Ord)

instance Show Order where
  show (Order u spc od) = show u <> " " <> spaceName spc <> " " <> show od

-- retreat phase orders
data RetreatOrder = RODisband Unit Space | RORetreat Unit Space Space deriving Eq

instance Show RetreatOrder where
  show (RODisband u spc) = show u <> " " <> show spc <> " disbands"
  show (RORetreat u spcFrom spcTo) = show u <> " " <> show spcFrom <> " retreats to " <> show spcTo

-- this is for printing the orders by country
instance Ord RetreatOrder where
  compare ro1 ro2 = case (compare (retreatOrderU ro1) (retreatOrderU ro2),
                          compare (retreatOrderAt ro1) (retreatOrderAt ro2),
                          compare (retreatOrderTo ro1) (retreatOrderTo ro2)) of
    (EQ, EQ, comp) -> comp
    (EQ, comp, _)  -> comp
    (comp, _, _)   -> comp

retreatOrderU :: RetreatOrder -> Unit
retreatOrderU (RODisband u _)   = u
retreatOrderU (RORetreat u _ _) = u

retreatOrderAt :: RetreatOrder -> Space
retreatOrderAt (RODisband _ spc)    = spc
retreatOrderAt (RORetreat _ spc _ ) = spc

retreatOrderTo :: RetreatOrder -> Maybe Space
retreatOrderTo (RODisband _ _)     = Nothing
retreatOrderTo (RORetreat _ _ spc) = Just spc

data BuildOrder = BODisband Country Space | BOBuild Country Space UnitType deriving Eq

instance Show BuildOrder where
  show (BOBuild c spc ut) = show c <> " builds " <> show spc <> " " <> show ut
  show (BODisband c spc)  = show c <> " disbands " <> show spc

buildOrderCountry :: BuildOrder -> Country
buildOrderCountry (BOBuild c _ _ ) = c
buildOrderCountry (BODisband c _ ) = c

buildOrderAt :: BuildOrder -> Space
buildOrderAt (BOBuild _ spc _ ) = spc
buildOrderAt (BODisband _ spc ) = spc

buildOrderUt :: BuildOrder -> Maybe UnitType
buildOrderUt (BOBuild _ _ ut) = Just ut
buildOrderUt (BODisband _ _ ) = Nothing

-- this is for printing the orders by country
instance Ord BuildOrder where
  compare bo1 bo2 = case (compare (buildOrderCountry bo1) (buildOrderCountry bo2),
                          compare (buildOrderAt bo1) (buildOrderAt bo2),
                          compare (buildOrderUt bo1) (buildOrderUt bo2)) of
    (EQ, EQ, comp) -> comp
    (EQ, comp, _)  -> comp
    (comp, _, _)   -> comp


