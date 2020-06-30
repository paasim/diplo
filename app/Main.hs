--{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Print
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  pn <- getProgName
  handleArgs args pn

handleArgs :: [String] -> String -> IO ()
handleArgs ("init":rest)           = handleInit rest
handleArgs ("board":rest)          = handleBoard rest
handleArgs ("state":rest)          = handleState rest
handleArgs ("orders":rest)         = handleOrders rest
handleArgs ("retreat-orders":rest) = handleRetreatOrders rest
handleArgs ("build-orders":rest)   = handleBuildOrders rest
handleArgs _ = printUsage "(init | board | state | orders | retreat-orders | build-orders)"

printUsage :: String -> String -> IO ()
printUsage suffix pn = putStrLn $ "usage: " <> pn <> " " <> suffix

handleInit :: [String] -> String -> IO ()
handleInit ["board"] _  = initBoard
handleInit ["state"] _  = initState
handleInit ["all"]   _  = initBoard >> initState
handleInit _ pn = printUsage "init (board | state | all)" pn 

handleBoard :: [String] -> String -> IO ()
handleBoard ("check":[fn]) _ = printValidatedBoard fn
handleBoard _ pn = printUsage "board check board.txt" pn 

handleState :: [String] -> String -> IO ()
handleState ("check":[fn]) _ = printValidatedState fn
handleState _ pn = printUsage "state check state.txt" pn 

handleOrders :: [String] -> String -> IO ()
handleOrders ("check":[fn])   pn = printValidatedOrders fn
handleOrders ("echo":[fn])    pn = printExecutableOrders fn
handleOrders ("execute":[fn]) pn = executeOrders fn
handleOrders _ pn = printUsage "orders (check | echo | execute) orders.txt" pn 

handleRetreatOrders :: [String] -> String -> IO ()
handleRetreatOrders ("check":[fn])   pn = printValidatedRetreatOrders fn
handleRetreatOrders ("echo":[fn])    pn = printExecutableRetreatOrders fn
handleRetreatOrders ("execute":[fn]) pn = executeRetreatOrders fn
handleRetreatOrders _ pn = printUsage "retreat-orders (check | echo | execute) orders.txt" pn 

handleBuildOrders :: [String] -> String -> IO ()
handleBuildOrders ("check":[fn])   pn = printValidatedBuildOrders fn
handleBuildOrders ("echo":[fn])    pn = printExecutableBuildOrders fn
handleBuildOrders ("execute":[fn]) pn = executeBuildOrders fn
handleBuildOrders _ pn = printUsage "build-orders (check | echo | execute) orders.txt" pn 

