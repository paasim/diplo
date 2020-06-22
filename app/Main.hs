--{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import InitialData 
import Board
import BState
import Orders
import Parse
import Validate
import Errors
import Utils
import RIO.List ( intercalate )
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  pn <- getProgName
  handleArgs args pn

handleArgs :: [String] -> String -> IO ()
handleArgs ["--init"]      _  = initBoardAndState
handleArgs ("--init":rest) pn = putStrLn $ "usage: " ++ pn ++ " --init"
handleArgs ["--board",fn]  _  = parseAndValidateBoard fn
handleArgs ("--board":_)   pn = putStrLn $ "usage: " ++ pn ++ " --board board.txt"
handleArgs ["--state",fn]  _  = parseAndValidateState fn
handleArgs ("--state":_)   pn = putStrLn $ "usage: " ++ pn ++ " --state state.txt"
handleArgs ["--orders",fn] _  = parseAndValidateOrders fn
handleArgs ("--orders":_)  pn = putStrLn $ "usage: " ++ pn ++ " --orders orders.txt"
handleArgs _               pn = putStrLn $
  "usage: " ++ pn ++ " --init | [--board | --state | --orders] filename.txt"

initBoardAndState :: IO ()
initBoardAndState = do
  writeFile "board.txt" (show initialBoard)
  writeFile "state.txt" (show initialState)

printValidatedT :: Show a => ValidatedT IO a -> IO ()
printValidatedT (ValidatedT ioa) = ioa >>= print

printValidatedList :: Show a => Validated [a] -> IO ()
printValidatedList (Valid l) = putStrLn . intercalate "\n" . fmap show $ l
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
  ValidatedT . return . validateOrders state $ orders

