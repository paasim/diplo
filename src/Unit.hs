{-# LANGUAGE NoImplicitPrelude #-}
module Unit
  ( Country (..)
  , UnitType (..)
  , Unit (..)
  ) where

import RIO

-- Units and related data
data UnitType = Fleet | Army deriving (Eq, Ord)

instance Show UnitType where
  show Fleet = "Fleet"
  show Army = "Army"

data Country = Austria | England | France | Germany | Italy | Russia | Turkey deriving (Eq, Ord)

instance Show Country where
  show Austria = "Austria"
  show England = "England"
  show France  = "France"
  show Germany = "Germany"
  show Italy   = "Italy"
  show Russia  = "Russia"
  show Turkey  = "Turkey"

data Unit = Unit { unitCountry :: Country, unitType :: UnitType } deriving (Eq, Ord)

instance Show Unit where
  show (Unit uc ut) = show uc <> " " <> show ut

