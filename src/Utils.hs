{-# LANGUAGE NoImplicitPrelude #-}
module Utils where

import Errors
import RIO
import qualified RIO.Map as M ( empty, insert, lookup )
import qualified RIO.Set as S ( empty, insert, member )

-- utility functions
toListChecker :: (a -> Validated a) -> [a] -> Validated [a]
toListChecker f xs = foldr (\x -> (<*>) ((:) <$> f x)) (Valid []) xs

setInsertNonExisting :: (Show a, Ord a) => a -> Set a -> Validated (Set a)
setInsertNonExisting a sa = case S.member a sa of
  True  -> ValidationError $ show a ++ " has multiple entries."
  False -> Valid $ S.insert a sa

mapInsertNonExisting :: (Show k, Ord k) => k -> v -> Map k v -> Validated (Map k v)
mapInsertNonExisting k v m = case M.lookup k m of
  (Just _) -> ValidationError $ show k ++ " has multiple entries."
  Nothing  -> Valid $ M.insert k v m

safeToSet :: (Show a, Ord a) => [a] -> Validated (Set a)
safeToSet []     = Valid S.empty
safeToSet (x:xs) = safeToSet xs >>= setInsertNonExisting x

safeToMap :: (Show k, Ord k) => [(k, v)] -> Validated (Map k v)
safeToMap []     = Valid M.empty
safeToMap ((k, v):rest) = safeToMap rest >>= mapInsertNonExisting k v

uncurry2 :: (a -> b -> c) -> (a, b) -> c
uncurry2 f (a,b) = f a b

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a,b,c) = f a b c

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a,b,c,d) = f a b c d

