--{-# LANGUAGE NoImplicitPrelude #-}
module Validate
  ( parseAndValidateBoard
  , parseAndValidateState
  , parseAndValidateOrders
  , printActiveOrders
  ) where

import Board
import BState
import Spaces
import Units
import Orders
import Parse
import Errors
import Utils
import RIO
import qualified RIO.NonEmpty as NE ( toList )
import qualified RIO.Map as M ( elems, empty, fromList, insert, lookup, toList )
import qualified RIO.Set as S ( empty, insert, member )
import System.Environment

-- actual validation
validateOrderUniqueness :: Set (Unit, Space) -> [Order] -> Validated [Order]
validateOrderUniqueness _ [] = Valid []
validateOrderUniqueness orderedUnits (o:os) = 
  let (u, s)      = getOrderedUnit o
      validatedOs = validateOrderUniqueness (S.insert (u, s) orderedUnits) os
  in case S.member (u, s) orderedUnits of
       False -> (:) o <$> validatedOs
       True  -> ValidationError $ "Multiple orders for '" ++ show u ++ "', '" ++ show s ++ "'."

validateOrders :: [Order] -> Validated [Order]
validateOrders = validateOrderUniqueness S.empty

-- parsing files and printing result
printValidatedT :: Show a => ValidatedT IO a -> IO ()
printValidatedT (ValidatedT ioa) = ioa >>= print

printValidatedList :: Show a => Validated [a] -> IO ()
printValidatedList (Valid l) = putStrLn . unlines . fmap show $ l
printValidatedList x         = print x

printValidatedTList :: Show a => ValidatedT IO [a] -> IO ()
printValidatedTList (ValidatedT ioa) = ioa >>= printValidatedList

parseAndValidateBoard' :: String -> ValidatedT IO Board
parseAndValidateBoard' fn = do
  boardData <- ValidatedT $ parseValidatedFromFile parseBoardData fn
  ValidatedT . return $ uncurry3 mkBoard boardData

parseAndValidateBoard :: String -> IO ()
parseAndValidateBoard = printValidatedT . parseAndValidateBoard'

parseAndValidateState' :: String -> Board -> ValidatedT IO BState
parseAndValidateState' fn board = do
  board <- parseAndValidateBoard' "board.txt"
  gameStateData <- ValidatedT $ parseValidatedFromFile (parseStateData board) fn
  ValidatedT . return $ uncurry4 (mkBState board) gameStateData

parseAndValidateState :: String -> IO ()
parseAndValidateState fn = printValidatedT $ parseAndValidateBoard' "board.txt" >>= parseAndValidateState' fn

parseAndValidateOrders :: String -> IO ()
parseAndValidateOrders fn = printValidatedTList $
  parseAndValidateBoard' "board.txt" >>= \b ->
    parseAndValidateState' "state.txt" b >>= parseAndValidateOrders' fn b

-- mapped to string to print list of orders in the
parseAndValidateOrders' :: String -> Board -> BState -> ValidatedT IO [Order]
parseAndValidateOrders' fn board state = do
  orders <- ValidatedT $ parseValidatedFromFile (parseOrders board state) fn
  ValidatedT . return . validateOrders $ orders


swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

initialOrders :: BState -> Map (Unit, Space) Order
initialOrders = M.fromList . fmap ((\x -> (x, uncurry Order x Hold)) . swap) . M.toList . occupiers

foldValidOrders :: [String] -> Map (Unit, Space) Order -> Board -> BState -> Map (Unit, Space) Order
foldValidOrders []                 m _     _     = m
foldValidOrders (orderString:rest) m board state = case parseValidated (parseOrder board state) orderString of
  Valid order -> foldValidOrders rest (M.insert (orderUnit order, orderSpace order) order m) board state
  _           -> foldValidOrders rest m board state

printActiveOrders' :: String -> ValidatedT IO [Order]
printActiveOrders' fn = do
  board <- parseAndValidateBoard' "board.txt"
  state <- parseAndValidateState' "state.txt" board
  orderStrings <- ValidatedT $ Valid . lines <$> readFile fn
  return . M.elems . foldValidOrders orderStrings (initialOrders state) board $ state

printActiveOrders :: String -> IO ()
printActiveOrders fn = printValidatedTList $ printActiveOrders' fn

