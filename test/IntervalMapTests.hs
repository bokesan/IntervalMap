-- module Data.IntervalMapTests (main) where

import System.Exit (exitSuccess, exitFailure)

import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)
import Data.List ((\\), sort, sortBy)
import Control.Monad (liftM, foldM)

import Data.IntervalMap as M
import Data.IntervalMap.Interval


newtype IMI = IMI (IntervalMap Int Int) deriving (Show)
newtype II = II (Interval Int) deriving (Eq, Ord, Show)

instance Arbitrary II where
  arbitrary = do x <- arbitrary
		 liftM II (interval (abs x))

interval :: Int -> Gen (Interval Int)
interval x = do
	     y <- sized (\n -> choose (x, x + abs n))
	     if x == y then return (ClosedInterval x y)
		else oneof [return (ClosedInterval x y),
			    return (OpenInterval x y),
			    return (IntervalCO x y),
			    return (IntervalOC x y)]

instance Arbitrary IMI where
  arbitrary = do xs <- orderedList
		 return (IMI (fromAscList [(v, lowerBound v) | (II v) <- xs]))


emptyM, single46 :: M.IntervalMap Int String
emptyM = M.empty
single46 = M.singleton (ClosedInterval 4 6) "single46"


prop_tests1 = 
  M.null emptyM &&
  0 == M.size emptyM &&
  1 == M.size single46 &&
  0 == M.height emptyM &&
  1 == M.height single46 &&
  Just "single46" == M.lookup (ClosedInterval 4 6) single46 &&
  "single46" == single46 M.! ClosedInterval 4 6 &&
  Nothing == M.lookup (OpenInterval 4 6) single46 &&
  [(ClosedInterval 4 6, "single46")] == single46 `containing` 5


bal3 :: M.IntervalMap Int String
bal3 =  let m1 = M.insert (ClosedInterval 1 4) "14" single46
	in M.insert (ClosedInterval 5 8) "58" m1

bal3' :: M.IntervalMap Int String
bal3' =  let m1 = M.insert (ClosedInterval 1 4) "14" single46
	 in M.insert (OpenInterval 5 8) "o58" m1

prop_tests2 =
   3 == M.size bal3 &&
   2 == M.height bal3 &&
  "single46" == bal3 M.! ClosedInterval 4 6 &&
  "14" == bal3 M.! ClosedInterval 1 4 &&
  "58" == bal3 M.! ClosedInterval 5 8 &&
  "o58" == bal3' M.! OpenInterval 5 8 &&
  Nothing == M.lookup (OpenInterval 5 8) bal3 &&
  Just "o58" == M.lookup (OpenInterval 5 8) bal3' &&
  [] == bal3 `containing` 0 &&
  [] == bal3 `containing` 9 &&
  [(ClosedInterval 1 4, "14")] == bal3 `containing` 1 &&
  [(ClosedInterval 5 8, "58")] == bal3 `containing` 8 &&
  [] == bal3' `containing` 8 &&
  [(OpenInterval 5 8, "o58")] == bal3' `containing` 7 &&
  sameElements ["14", "single46"] [v|(_,v) <- bal3 `containing` 4] &&
  sameElements ["58", "single46"] [v|(_,v) <- bal3 `containing` 5] &&
  sameElements ["58", "single46"] [v|(_,v) <- bal3 `containing` 6] &&
  sameElements ["single46"] [v|(_,v) <- bal3' `containing` 5] &&
  sameElements ["o58", "single46"] [v|(_,v) <- bal3' `containing` 6]


deep100L :: M.IntervalMap Int Int
deep100L = construct 100 M.empty
  where construct n m  | n <= 0    = m
		       | otherwise = construct (n - 1) (M.insert (ClosedInterval n n) n m)

deep100R :: M.IntervalMap Int Int
deep100R = construct 1 M.empty
  where construct n m  | n > 100   = m
		       | otherwise = construct (n + 1) (M.insert (ClosedInterval n n) n m)


prop_tests3 =
   68 == deep100L M.! (ClosedInterval 68 68) &&
   [17] == Prelude.map snd (deep100L `containing` 17) &&
   100 == M.size deep100L &&
   (M.height deep100L <= 12) &&
   68 == deep100R M.! (ClosedInterval 68 68) &&
   [17] == Prelude.map snd (deep100R `containing` 17) &&
   100 == M.size deep100R &&
   (M.height deep100R <= 12) &&
   M.valid deep100L &&
   M.valid deep100R &&
   99 == M.size (M.delete (ClosedInterval 23 23) deep100L) &&
   99 == M.size (M.delete (ClosedInterval 23 23) deep100R) &&
   M.valid (M.delete (ClosedInterval 23 23) deep100L) &&
   M.valid (M.delete (ClosedInterval 23 23) deep100R)

prop_mapKeys =
		equalMap ["foo"] (M.mapKeys lower (M.insert (ClosedInterval 4 5) "foo" single46)) &&
		equalMap ["single46"] (M.mapKeys lower (M.insert (ClosedInterval 4 7) "foo" single46))
  where lower k = ClosedInterval (lowerBound k) (lowerBound k)

prop_mapKeysWith (IMI m) = M.valid m' && all correct (M.keys m)
  where lower k = ClosedInterval (lowerBound k) (lowerBound k)
	m' = M.mapKeysWith (+) lower m
	correct x = let mps = sum [v | (k,v) <- M.toList m, lowerBound k == lowerBound x]
		    in case M.lookup (lower x) m' of
		         Nothing -> False
			 Just v' -> v' == mps
			
	
    

-- check that our generator yields valid maps.
prop_valid (IMI m) = M.valid m

prop_delete (IMI m) (II k) = let m' = M.delete k m in
			     M.valid m' &&
			     notMember k m' &&
			     if M.null m          then M.null m'
			     else if M.member k m then M.size m' == M.size m - 1
			     else                      M.size m' == M.size m

prop_insert (IMI m) (II k) = let m' = M.insert k 4711 m in
                             M.valid m' &&
			     M.lookup k m' == Just 4711 &&
			     if M.member k m then M.size m' == M.size m
				             else M.size m' == M.size m + 1

prop_min (IMI m) = if M.null m then M.null (M.deleteMin m) else
                                      let (k,v) = findMin m
					  m' = deleteMin m
				      in notMember k m' && M.size m == M.size m' + 1
					 && k == minimum (M.keys m) && valid m'

prop_max (IMI m) = if M.null m then M.null (M.deleteMax m) else
                                      let (k,v) = findMax m
					  m' = deleteMax m
				      in notMember k m' && M.size m == M.size m' + 1
					 && k == maximum (M.keys m) && valid m'

prop_updateMin_u (IMI m) =
   let m' = M.updateMin (\v -> Just (v+1)) m in
   if M.null m then
      M.null m'
   else
      let (k, v) = M.findMin m
          (k', v') = M.findMin m'
      in
      M.valid m' &&
      M.size m' == M.size m &&
      k' == k &&
      v' == v + 1

prop_updateMin_d (IMI m) =
   let m' = M.updateMin (const Nothing) m in
   if M.null m then
      M.null m'
   else
      let (k,v) = M.findMin m in
      M.valid m' &&
      M.size m' == M.size m - 1 &&
      M.notMember k m'

prop_updateMax_u (IMI m) =
   let m' = M.updateMax (\v -> Just (v+1)) m in
   if M.null m then
      M.null m'
   else
      let (k, v) = M.findMax m
          (k', v') = M.findMax m'
      in
      M.valid m' &&
      M.size m' == M.size m &&
      k' == k &&
      v' == v + 1

prop_updateMax_d (IMI m) =
   let m' = M.updateMax (const Nothing) m in
   if M.null m then
      M.null m'
   else
      let (k,v) = M.findMax m in
      M.valid m' &&
      M.size m' == M.size m - 1 &&
      M.notMember k m'

prop_map (IMI m) = let m' = M.map (1+) m in
                    M.valid m' &&
                    M.size m' == M.size m &&
                    all (\k -> m' M.! k == m M.! k + 1) (M.keys m)

prop_findWithDefault (IMI m) (II k) = M.findWithDefault (lowerBound k) k m == lowerBound k

prop_searchPoint (IMI m) p = sameElements (m `containing` p)
                                          [e | e@(k,_) <- M.toList m, p `inside` k]

prop_searchInterval (IMI m) (II i) = sameElements (m `intersecting` i)
				                  [e | e@(k,_) <- M.toList m, k `overlaps` i]

prop_within (IMI m) (II i) = sameElements (m `M.within` i)
                                          [e | e@(k,_) <- M.toList m, i `subsumes` k]

prop_findMin (IMI m) = not (M.null m) ==> let x = minimum (M.toList m)
					      (y,m') = M.deleteFindMin m
					  in M.findMin m == x &&
					     y == x &&
					     M.valid m' &&
					     sameElements (M.toList m Data.List.\\ [x]) (M.toList m') &&
					     sameElements (M.toList m Data.List.\\ [x]) (M.toList (M.deleteMin m))

prop_findMax (IMI m) = not (M.null m) ==> let x = maximum (M.toList m)
					      (y,m') = M.deleteFindMax m
					  in M.findMax m == x &&
					     y == x &&
					     M.valid m' &&
					     sameElements (M.toList m Data.List.\\ [x]) (M.toList m') &&
					     sameElements (M.toList m Data.List.\\ [x]) (M.toList (M.deleteMax m))

prop_findLast (IMI m) = not (M.null m) ==>
                         M.findLast m == head (sortBy cmp (M.toList m))
                        where cmp (a,_) (b,_) = invert (compareByUpper a b)
                              invert LT = GT
                              invert GT = LT
                              invert EQ = EQ


prop_insertWith (IMI m) (II i) v = let m' = M.insertWith (\new old -> new + old) i v m in
                                   if M.member i m then
                                      M.valid m' && m' M.! i == m M.! i + v && M.size m' == M.size m
				   else
				      M.valid m' && m' M.! i == v && M.size m' == M.size m + 1

prop_insertWith' (IMI m) (II i) v = let m' = M.insertWith' (\new old -> new + old) i v m in
                                    if M.member i m then
                                       M.valid m' && m' M.! i == m M.! i + v && M.size m' == M.size m
		 		    else
		 		       M.valid m' && m' M.! i == v && M.size m' == M.size m + 1

prop_insertLookupWithKey (IMI m) (II i) v =
  case M.insertLookupWithKey (\k new old -> upperBound k + new + old) i v m of
    (Nothing, m')  -> M.valid m' && M.notMember i m && m' M.! i == v
    (Just old, m') -> M.valid m' && m M.! i == old && m' M.! i == upperBound i + v + old

prop_insertLookupWithKey' (IMI m) (II i) v =
  case M.insertLookupWithKey' (\k new old -> upperBound k + new + old) i v m of
    (Nothing, m')  -> M.valid m' && M.notMember i m && m' M.! i == v
    (Just old, m') -> M.valid m' && m M.! i == old && m' M.! i == upperBound i + v + old


prop_foldr (IMI m) = M.foldr f z m == Prelude.foldr f z [ v | (_,v) <- M.toAscList m ]
  where z = []
	f = (:)

prop_adjust (II i) (IMI m) = let m' = M.adjust (13*) i m in
			     M.valid m' &&
                             case M.lookup i m of
                               Nothing -> m == m'
			       Just v -> case M.lookup i m' of
					   Nothing -> False
					   Just v' -> v' == v * 13

prop_update (II i) (IMI m) = let f n = if n `rem` 2 == 0 then Nothing else Just (13 * n)
				 m' = M.update f i m
			     in
			         M.valid m' &&
			         case M.lookup i m of
				   Nothing -> m == m'
				   Just v -> case M.lookup i m' of
					       Nothing -> v `rem` 2 == 0
					       Just v' -> v' == 13 * v

prop_alter (IMI m) (II k) = delete && insert
  where
    delete = let m' = M.alter (const Nothing) k m in M.valid m' && M.notMember k m'
    insert = let m' = M.alter (const (Just 4711)) k m in M.valid m' && M.lookup k m' == Just 4711


prop_union (IMI m1) (IMI m2) =  M.size m' == M.size m1 + numNotInM1 0 (M.keys m2) -- size
                                && valsM1 (M.assocs m1) -- m1 entries unchanged
				&& valsM2 (M.assocs m2) -- m2 entries not in m1 unchanged
				&& M.valid m'
  where
    m' = m1 `M.union` m2

    valsM1 [] = True
    valsM1 ((k,v):xs) = case M.lookup k m' of
			  Nothing -> False
			  Just v' -> v' == v && valsM1 xs

    valsM2 [] = True
    valsM2 ((k,v):xs) | M.member k m1 = valsM2 xs
		      | otherwise = case M.lookup k m' of
				      Nothing -> False
				      Just v' -> v' == v && valsM2 xs

    numNotInM1 n [] = n
    numNotInM1 n (k:ks) | M.member k m1 = numNotInM1 n ks
			| otherwise     = numNotInM1 (n+1) ks
    

prop_unionWithKey (IMI m1) (IMI m2) =  M.size m' == M.size m1 + numNotInM1 0 (M.keys m2) -- size
                                       && valuesCorrect (M.assocs m')
				       && M.valid m'
  where
    f k a b = 7 * upperBound k + 3 * b + b
    m' = M.unionWithKey f m1 m2

    valuesCorrect [] = True
    valuesCorrect ((k,v):xs) = case M.lookup k m1 of
			         Nothing -> case M.lookup k m2 of
					      Nothing -> False
					      Just v2 -> v2 == v && valuesCorrect xs
				 Just v1 -> case M.lookup k m2 of
					      Nothing -> v1 == v && valuesCorrect xs
					      Just v2 -> v == f k v1 v2 && valuesCorrect xs
    numNotInM1 n [] = n
    numNotInM1 n (k:ks) | M.member k m1 = numNotInM1 n ks
			| otherwise     = numNotInM1 (n+1) ks
    

prop_unions (IMI m1) (IMI m2) (IMI m3) = M.unions [m1,m2,m3] == (m1 `M.union` m2 `M.union` m3)

prop_difference (IMI m1) (IMI m2) =  M.valid m' && m' == Prelude.foldr M.delete m1 (M.keys m2)
  where m' = m1 M.\\ m2

prop_intersection (IMI m1) (IMI m2) = M.valid m' && all inBoth (M.keys m')
  where
    m' = M.intersection m1 m2
    inBoth k = M.member k m1 && M.member k m2

prop_fromAscList :: [(II,Int)] -> Bool
prop_fromAscList lyst = M.valid m && all correctVal [k | (II k, _) <- lyst]
  where
    xs = sort [(k,v) | (II k,v) <- lyst]
    rs = reverse xs
    m = M.fromAscList xs
    correctVal k = case assoc k rs of
                     Nothing -> False
                     Just v  -> m M.! k == v

assoc :: Eq k => k -> [(k,a)] -> Maybe a
assoc _ [] = Nothing
assoc k ((x,v):xs) | x == k    = Just v
		   | otherwise = assoc k xs

prop_mapAccum (IMI m) =  M.valid m' && acc == sum (M.elems m) && sum (M.elems m') == 2 * acc
  where (acc, m') = M.mapAccum (\a v -> (a+v, 2*v)) 0 m

-- filter

prop_filter (IMI m) =  M.valid m' && all odd (M.elems m') && M.size m' == odds
  where
    m' = M.filter odd m
    odds = length [x | x <- M.elems m, odd x]

prop_partition (IMI m) =  M.valid m1 && M.valid m2 && all odd (M.elems m1) && all even (M.elems m2)
			  && M.size m == M.size m1 + M.size m2
   where
     (m1,m2) = M.partition odd m


prop_splitLookup (IMI m) (II x) = M.valid l && M.valid r
				  && all (< x) (M.keys l) && all (> x) (M.keys r)
				  && value == M.lookup x m
				  && M.size m == M.size l + M.size r + (if M.member x m then 1 else 0)
  where
    (l, value, r) = splitLookup x m


checkElems :: Int -> Int -> [(Interval Int, Int)] -> Bool
checkElems n len lyst = h n (n + len) lyst
  where
    h i n xs | i > n  = True
             | otherwise = case xs of ((iv,v):xs') -> if v == i && lowerBound iv == i && upperBound iv == i
						      then h (i+1) n xs'
						      else False


check p name = do r <- quickCheckWithResult (stdArgs { maxSuccess = 500 }) p
		  if isSuccess r
		   then return r
		   else do putStrLn ("error: " ++ name ++ ": " ++ show r)
			   exitFailure


main :: IO ()
main = do
          check prop_tests1 "tests1"
          check prop_tests2 "tests2"
          check prop_tests3 "tests3"
          check prop_mapKeys "mapKeys"
	  check prop_valid "valid"
	  check prop_delete "delete"
	  check prop_insert "insert"
	  check prop_min "min"
	  check prop_max "max"
	  check prop_findWithDefault "findWithDefault"
	  check prop_searchPoint "searchPoint"
	  check prop_searchInterval "searchInterval"
          check prop_within "within"
	  check prop_findMin "findMin"
	  check prop_findMax "findMax"
          check prop_updateMin_u "updateMin update"
          check prop_updateMin_d "updateMin delete"
          check prop_updateMax_u "updateMax update"
          check prop_updateMax_d "updateMax delete"
	  check prop_insertWith "insertWith"
          check prop_insertWith' "insertWith'"
	  check prop_insertLookupWithKey "insertLookupWithKey"
	  check prop_insertLookupWithKey' "insertLookupWithKey'"
	  check prop_map "map"
	  check prop_foldr "foldr"
	  check prop_fromAscList "fromAscList"
	  check prop_adjust "adjust"
	  check prop_update "update"
	  check prop_union "union"
	  check prop_unionWithKey "unionWithKey"
	  check prop_unions "unions"
	  check prop_difference "difference"
	  check prop_intersection "intersection"
	  check prop_filter "filter"
	  check prop_partition "partition"
	  check prop_splitLookup "splitLookup"
	  check prop_mapKeysWith "mapKeysWith"
	  putStrLn ("deep100L: " ++ show (M.showStats deep100L))
	  putStrLn ("deep100R: " ++ show (M.showStats deep100R))
	  exitSuccess

-- Utils -----------------

equalMap :: (Eq a) => [a] -> IntervalMap k a -> Bool
equalMap xs m = sameElements xs (M.elems m)

sameElements :: Eq a => [a] -> [a] -> Bool
sameElements []     []    = True
sameElements []     (_:_) = False
sameElements (x:xs) ys    = case tryRemove x ys of
			      Nothing  -> False
			      Just ys' -> sameElements xs ys'
  where
    tryRemove _ [] = Nothing
    tryRemove x (y:ys) | x == y    = Just ys
		       | otherwise = case tryRemove x ys of
				       Nothing  -> Nothing
				       Just ys' -> Just (y : ys')
