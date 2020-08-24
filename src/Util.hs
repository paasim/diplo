{-# LANGUAGE NoImplicitPrelude #-}
module Util where

import Error
import RIO
import qualified RIO.List as L
import qualified RIO.Map as M
import qualified RIO.Set as S

-- utility functions
showPair :: (Show a, Show b) => (a, b) -> String
showPair (a, b) = show a <> " " <> show b

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

ungroupSnd :: [(a, [b])] -> [(a, b)]
ungroupSnd l = l >>= sequenceA

ungroupFst :: [([a], b)] -> [(a, b)]
ungroupFst = fmap swap . join . fmap sequenceA . fmap swap -- unnests [prov]

toListChecker :: (a -> Validated a) -> [a] -> Validated [a]
toListChecker f = foldr (\x -> (<*>) ((:) <$> f x)) (Valid [])

setInsertNonExisting :: (Show a, Ord a) => a -> Set a -> Validated (Set a)
setInsertNonExisting a sa = case S.member a sa of
  True  -> ValidationError $ show a <> " has multiple entries."
  False -> Valid $ S.insert a sa

mapInsertNonExisting :: (Show k, Ord k) => k -> v -> Map k v -> Validated (Map k v)
mapInsertNonExisting k v m = case M.member k m of
  True  -> ValidationError $ show k <> " has multiple entries."
  False -> Valid $ M.insert k v m

safeToSet :: (Show a, Ord a) => [a] -> Validated (Set a)
safeToSet = foldl' (\vsa x -> vsa >>= setInsertNonExisting x) (Valid S.empty)

safeToMap :: (Show k, Ord k) => [(k, v)] -> Validated (Map k v)
safeToMap = foldl' (\vmkv (k, v) -> vmkv >>= mapInsertNonExisting k v) (Valid M.empty)

getUnique :: (a -> Bool) -> Set a -> Maybe a
getUnique f sa = let matching = S.filter f sa in case S.size matching of
  1 -> S.lookupMin matching
  _ -> Nothing

insertNew :: Ord a => a -> [(a, Int)] -> [(a, Int)]
insertNew aNew []           = [(aNew, 1)]
insertNew aNew ((a,i):rest) = case compare aNew a of
  LT -> (aNew,1) : (a,i) : rest
  EQ -> (a,i+1) : rest
  GT -> (a,i) : insertNew aNew rest

counts :: Ord a => [a] -> [(a, Int)]
counts = foldr insertNew [] . L.sort

getDuplicates :: Ord a => [a] -> Set a
getDuplicates = S.fromList . fmap fst . filter (\(a, i) -> i > 1) . counts

getUniques :: Ord a => [a] -> [a]
getUniques = fmap fst . counts

filterJust :: [(a, Maybe b)] -> [(a, b)]
filterJust = catMaybes . fmap sequenceA

uncurry2 :: (a -> b -> c) -> (a, b) -> c
uncurry2 f (a,b) = f a b

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a,b,c) = f a b c

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a,b,c,d) = f a b c d

uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 f (a,b,c,d,e) = f a b c d e


