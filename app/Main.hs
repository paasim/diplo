--{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import InitialData 
import Validate
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
handleArgs ["--active-orders",fn] _  = printActiveOrders fn
handleArgs ("--active-orders":_)  pn = putStrLn $ "usage: " ++ pn ++ " --active-orders orders.txt"
handleArgs _               pn = putStrLn $
  "usage: " ++ pn ++ " --init | [--board | --state | --orders | --active-orders] filename.txt"

initBoardAndState :: IO ()
initBoardAndState = do
  writeFile "board.txt" (show initialBoard)
  writeFile "state.txt" (show initialState)

