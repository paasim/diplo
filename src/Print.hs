--{-# LANGUAGE NoImplicitPrelude #-}
module Print 
  ( initBoard
  , initState
  , printValidatedBoard
  , printValidatedState
  , printValidatedOrders
  , printExecutableOrders
  , executeOrders
  , printValidatedRetreatOrders
  , printExecutableRetreatOrders
  , executeRetreatOrders
  , printValidatedBuildOrders
  , printExecutableBuildOrders
  , executeBuildOrders
  ) where

import Error
import Util
import Province
import Order
import Board
import BState
import UpdateState
import Parse
import Validate
import InitialData 
import RIO
import qualified RIO.List as L
import System.Environment


boardFn = "board.txt"
stateFn = "state.txt"

initBoard = writeFile boardFn (show initialBoard)
initState = writeFile stateFn (show initialState)


-- parsing files and printing result
printValidatedT :: Show a => ValidatedT IO a -> IO ()
printValidatedT (ValidatedT ioa) = ioa >>= print

printValidatedList :: Show a => Validated [a] -> IO ()
printValidatedList (Valid l) = putStrLn . unlines . fmap show $ l
printValidatedList x         = print x

printValidatedTList :: Show a => ValidatedT IO [a] -> IO ()
printValidatedTList (ValidatedT ioa) = ioa >>= printValidatedList


-- board
validateBoard:: String -> ValidatedT IO Board
validateBoard fn = do
  boardData <- ValidatedT $ parseValidatedFromFile parseBoardData fn
  ValidatedT . return $ uncurry3 mkBoard boardData

printValidatedBoard :: String -> IO ()
printValidatedBoard = printValidatedT . validateBoard


-- state
validateState :: String -> Board -> ValidatedT IO BState
validateState fn board = do
  gameStateData <- ValidatedT $ parseValidatedFromFile (parseStateData board) fn
  ValidatedT . return $ uncurry4 (mkBState board) gameStateData

printValidatedState :: String -> IO ()
printValidatedState fn = printValidatedT $ validateBoard boardFn >>= validateState fn


-- orders
validateOrders :: String -> BState -> ValidatedT IO [Order]
validateOrders fn state = do
  orders <- ValidatedT $ parseValidatedFromFile (parseOrders state) fn
  ValidatedT . return . checkDuplicates $ orders

printValidatedOrders :: String -> IO ()
printValidatedOrders fn = printValidatedTList $
  validateBoard boardFn >>= validateState stateFn >>= validateOrders fn

-- executable orders
validateOrdersWithDefault :: String -> ValidatedT IO (BState, [Order])
validateOrdersWithDefault fn = do
  board <- validateBoard boardFn
  state <- validateState stateFn board
  orderStrings <- ValidatedT . fmap (Valid . lines) . readFile $ fn
  orders <- ValidatedT . return . getExecutableOrders state $ orderStrings
  return (state, L.sort orders)

printExecutableOrders :: String -> IO ()
printExecutableOrders = printValidatedTList . fmap snd . validateOrdersWithDefault

-- execute orders
executeOrders :: String -> IO ()
executeOrders = printValidatedT . joinInsideValidatedT . fmap (uncurry applyMovements) . validateOrdersWithDefault


-- retreat orders
validateRetreatOrders :: String -> BState -> ValidatedT IO [RetreatOrder]
validateRetreatOrders fn state =
  ValidatedT $ parseValidatedFromFile (parseRetreatOrders state) fn

printValidatedRetreatOrders :: String -> IO () 
printValidatedRetreatOrders fn = printValidatedTList $
  validateBoard boardFn >>= validateState stateFn >>= validateRetreatOrders fn

-- executable retreat orders
validateRetreatOrdersWithDefault :: String -> ValidatedT IO (BState, [RetreatOrder])
validateRetreatOrdersWithDefault fn = do
  board <- validateBoard boardFn
  state <- validateState stateFn board
  retreatStrings <- ValidatedT . fmap (Valid . lines) . readFile $ fn
  retreatOrders <- ValidatedT . return . getExecutableRetreatOrders state $ retreatStrings
  return (state, L.sort retreatOrders)

printExecutableRetreatOrders :: String -> IO ()
printExecutableRetreatOrders = printValidatedTList . fmap snd . validateRetreatOrdersWithDefault 

-- execute retreat orders
executeRetreatOrders :: String -> IO ()
executeRetreatOrders = printValidatedT . joinInsideValidatedT . fmap (uncurry applyRetreats) . validateRetreatOrdersWithDefault


-- build orders
validateWinter :: BState -> ValidatedT IO BState
validateWinter state = case season . gamePhase $ state of
  Winter -> ValidatedT . return . Valid $ state
  phase  -> ValidatedT . return . ValidationError $ "Build phase cannot happen in " <> show phase

validateBuildOrders :: String -> BState -> ValidatedT IO [BuildOrder]
validateBuildOrders fn state =
  ValidatedT $ parseValidatedFromFile (parseBuildOrders state) fn

printValidatedBuildOrders :: String -> IO () 
printValidatedBuildOrders fn = printValidatedTList $
  validateBoard boardFn >>= validateState stateFn >>= validateWinter >>= validateBuildOrders fn

-- executable build orders
validateBuildOrdersWithDefault :: String -> ValidatedT IO (BState, [BuildOrder])
validateBuildOrdersWithDefault fn = do
  board <- validateBoard boardFn
  state <- validateState stateFn board >>= validateWinter
  buildStrings <- ValidatedT . fmap (Valid . lines) . readFile $ fn
  buildOrders <- ValidatedT . return . getExecutableBuildOrders state $ buildStrings
  return (state, L.sort buildOrders)

printExecutableBuildOrders :: String -> IO ()
printExecutableBuildOrders = printValidatedTList . fmap snd . validateBuildOrdersWithDefault 

-- execute build orders
executeBuildOrders :: String -> IO ()
executeBuildOrders = printValidatedT . joinInsideValidatedT . fmap (uncurry applyBuilds) . validateBuildOrdersWithDefault


