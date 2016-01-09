{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- module IntervalSetTests (main) where

import System.Exit (exitSuccess, exitFailure)

import Test.QuickCheck hiding (within)
import Test.QuickCheck.Test (isSuccess)
import Control.Monad (liftM)
import Prelude hiding (null, map, filter, foldr, foldl, splitAt)
import qualified Data.List as L

import Data.IntervalSet


data II = II !Int !Int deriving (Eq, Ord, Show, Read)

bump :: Int -> II -> II
bump n (II a b) = II (a+n) (b+n)

contains :: II -> Int -> Bool
contains (II a b) n = a <= n && n <= b

instance Interval II Int where
   lowerBound (II a _) = a
   upperBound (II _ b) = b

combine :: II -> II -> Maybe II
combine i1@(II a b) i2@(II c d) | i1 `overlaps` i2 = Just (II (min a c) (max b d))
                                | otherwise = Nothing

instance Arbitrary II where
  arbitrary = do x <- arbitrary
                 iv <- interval (abs x)
                 return iv

interval :: Int -> Gen II
interval x = do y <- sized (\n -> choose (x, x + abs n))
                return (II x y)

newtype IS = IS (IntervalSet II) deriving (Show)
                       
instance Arbitrary IS where
  arbitrary = do xs <- orderedList
                 return (IS (fromAscList xs))

prop_valid (IS s) =               valid s

prop_null (IS s) =                null s == (size s == 0)

prop_size (IS s) =                size s == length (toList s)

prop_singleton :: II -> Bool
prop_singleton iv =               size (singleton iv) == 1
                                  
prop_insert_member (IS s) iv =    member iv (insert iv s)

prop_notMember (IS s) iv =        notMember iv s == not (member iv s)

prop_isSubsetOf (IS s1) (IS s2) = (s1 `isSubsetOf` s2) == L.null (toList s1 L.\\ toList s2)

prop_isProperSubsetOf (IS s1) (IS s2)
                                = (s1 `isProperSubsetOf` s2) ==
                                   (L.null (toList s1 L.\\ toList s2) && s1 /= s2)
                                  
prop_insert_size (IS s) iv =
    let s' = insert iv s in
    if member iv s
      then size s' == size s
      else size s' == size s + 1

prop_delete (IS s) iv =           let s' = delete iv s in
                                  valid s' &&
                                  if notMember iv s then s' == s
                                                    else all (\e -> e `member` s' || e == iv) (toList s)

prop_list (IS s) =                s == fromList (toList s)
prop_asclist (IS s) =             s == fromAscList (toAscList s)
prop_desclist (IS s) =            toDescList s == reverse (toAscList s)
prop_elems (IS s) =               elems s == toAscList s

prop_union (IS s1) (IS s2) =      union s1 s2 == foldr insert s2 s1

prop_unions xs  =                 unions iss == L.foldl union empty iss
                                  where iss = [is | IS is <- xs]

prop_difference (IS s1) (IS s2) = (s1 \\ s2) == foldr delete s1 s2
                                      
prop_intersection (IS s1) (IS s2) =
                                  let i = intersection s1 s2 in
                                  valid i &&
                                  all (\e -> member e s1 && member e s2) (toList i)
                                      
prop_minView (IS s) =             case minView s of
                                    Nothing -> null s
                                    Just (min, s') -> valid s' && all (min <) (toList s')
                                  
prop_maxView (IS s) =             case maxView s of
                                    Nothing -> null s
                                    Just (max, s') -> valid s' && all (max >) (toList s')
                                    
prop_findMin (IS s) =             case findMin s of
                                    Nothing  -> null s
                                    Just min -> all (min <=) (toList s)

prop_findMax (IS s) =             case findMax s of
                                     Nothing  -> null s
                                     Just max -> all (max >=) (toList s)

prop_findLast (IS s) =            case findLast s of
                                    Nothing -> null s
                                    Just x@(II _ end) ->
                                      all (\e -> upperBound e < end || (upperBound e == end && e <= x)) (toList s)
                                                      
prop_deleteMin (IS s) =           let s' = deleteMin s in
                                  valid s' &&
                                  case findMin s of
                                    Nothing  -> null s'
                                    Just min -> s' == delete min s

prop_deleteMax (IS s) =           let s' = deleteMax s in
                                  valid s' &&
                                  case findMax s of
                                    Nothing  -> null s'
                                    Just max -> s' == delete max s
                                                      
prop_map (IS s) n =               s == map (bump n) (map (bump (-n)) s)
prop_mapMonotonic (IS s) n =      s == mapMonotonic (bump n) (mapMonotonic (bump (-n)) s)

prop_filter (IS s) iv =           filter (iv /=) s == delete iv s

prop_partition (IS s) iv =        let (lo,hi) = partition (<= iv) s in
                                  valid lo && valid hi &&
                                  all (<= iv) (toList lo) &&
                                  all (> iv) (toList hi) &&
                                  union lo hi == s

prop_split (IS s) iv =            let (lo,hi) = split iv s in
                                  all (< iv) (toList lo) &&
                                  all (> iv) (toList hi) &&
                                  union lo hi == if member iv s then delete iv s else s

prop_splitMember (IS s) iv =      let (lo,m,hi) = splitMember iv s in
                                  valid lo && valid hi &&
                                  m == member iv s &&
                                  all (< iv) (toList lo) &&
                                  all (> iv) (toList hi) &&
                                  union lo hi == if m then delete iv s else s

prop_splitAt1 p (IS s) =          let (lo,_,_) = splitAt s p in
                                  valid lo && lo == filter (p `above`) s

prop_splitAt2 p (IS s) =          let (_,c,_) = splitAt s p in
                                  valid c && c == filter (p `inside`) s

prop_splitAt3 p (IS s) =          let (_,_,hi) = splitAt s p in
                                  valid hi && hi == filter (p `below`) s

prop_splitIntersecting i (IS s) = let (lo,c,hi) = splitIntersecting s i in
                                  valid lo && valid c && valid hi &&
                                  lo == filter (i `after`) s &&
                                  c  == filter (i `overlaps`) s &&
                                  hi == filter (i `before`) s &&
                                  unions [lo,c,hi] == s &&
                                  size lo + size c + size hi == size s

prop_readShow (IS s) =            s == read (show s)


prop_containing :: IS -> Int -> Bool
prop_containing (IS s) n =        let s' = s `containing` n in
                                  valid s' &&
                                  all (\e -> if e `contains` n then e `member` s' else e `notMember` s') (toList s)

prop_intersecting :: IS -> II -> Bool
prop_intersecting (IS s) iv =     let s' = s `intersecting` iv in
                                  valid s' &&
                                  all (\e -> if e `overlaps` iv then e `member` s' else e `notMember` s') (toList s)
                                      
prop_within :: IS -> II -> Bool
prop_within (IS s) iv =           let s' = s `within` iv in
                                  valid s' &&
                                  all (\e -> if iv `subsumes` e then e `member` s' else e `notMember` s') (toList s)
                                  
prop_foldr  (IS s) iv =           Just (foldr  (\v r -> min v r) iv s) == findMin (insert iv s)
prop_foldr' (IS s) iv =           Just (foldr' (\v r -> min v r) iv s) == findMin (insert iv s)
prop_foldl  (IS s) iv =           Just (foldl  (\r v -> min v r) iv s) == findMin (insert iv s)
prop_foldl' (IS s) iv =           Just (foldl' (\r v -> min v r) iv s) == findMin (insert iv s)

prop_flattenWithMonotonic (IS s) = let s' = flattenWithMonotonic combine s in
                                   valid s' &&
                                   size s' <= size s &&
                                   nonOverlapping (toAscList s') &&
                                   (null s || (let Just a = findMin s
                                                   Just b = findMin s'
                                                   Just c = findLast s
                                                   Just d = findLast s'
                                               in lowerBound a == lowerBound b &&
                                                  upperBound c == upperBound d))

nonOverlapping :: (Interval i e) => [i] -> Bool
nonOverlapping (x:y:xs) | x `overlaps` y = False
                        | otherwise      = nonOverlapping (y:xs)
nonOverlapping _ = True

check p name = do putStrLn ("Testing " ++ name ++ ":")
                  r <- quickCheckWithResult (stdArgs { maxSuccess = 500 }) p
                  if isSuccess r
                   then return r
                   else do putStrLn ("error: " ++ name ++ ": " ++ show r)
                           exitFailure


main = do
         check prop_valid "valid"
         check prop_null "null"
         check prop_size "size"
         check prop_notMember "notMember"
         check prop_singleton "singleton"
         check prop_isSubsetOf "subsetOf"
         check prop_isProperSubsetOf "properSubsetOf"
         check prop_insert_member "insert -> member"
         check prop_insert_size "insert + size"
         check prop_delete "delete"
         check prop_list "toList/fromList"
         check prop_asclist "toAscList/fromAscList"
         check prop_desclist "toDescList"
         check prop_elems "elems"
         check prop_union "union"
         check prop_unions "unions"
         check prop_difference "difference"
         check prop_intersection "intersection"
         check prop_findMin "findMin"
         check prop_findMax "findMax"
         check prop_findLast "findLast"
         check prop_deleteMin "deleteMin"
         check prop_deleteMax "deleteMax"
         check prop_minView "minView"
         check prop_maxView "maxView"
         check prop_foldr  "foldr"
         check prop_foldr' "foldr'"
         check prop_foldl  "foldl"
         check prop_foldl' "foldl'"
         check prop_map "map"
         check prop_mapMonotonic "mapMonotonic"
         check prop_filter "filter"
         check prop_partition "partition"
         check prop_split "split"
         check prop_splitMember "splitMember"
         check prop_splitAt1 "splitAt lower"
         check prop_splitAt2 "splitAt containing"
         check prop_splitAt3 "splitAt higher"
         check prop_splitIntersecting "splitIntersecting"
         check prop_containing "containing"
         check prop_intersecting "intersecting"
         check prop_within "within"
         check prop_flattenWithMonotonic "flattenWithMonotonic"
         check prop_readShow "read/show"
         exitSuccess
