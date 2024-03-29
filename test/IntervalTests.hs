-- module IntervalTests (main) where

import System.Exit (exitSuccess, exitFailure)

import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)
import Data.List (maximumBy)
import Data.Maybe

import Data.IntervalMap.Interval


c15, o15, co15, oc15 :: Interval Int
c15 = ClosedInterval 1 5
o15 = OpenInterval 1 5
co15 = IntervalCO 1 5
oc15 = IntervalOC 1 5


prop_boundsC, prop_boundsO, prop_boundsCO, prop_boundsOC :: Int -> Int -> Bool
prop_boundsC lo hi = let iv = ClosedInterval lo hi in lowerBound iv == lo && upperBound iv == hi
prop_boundsO lo hi = let iv = OpenInterval lo hi in lowerBound iv == lo && upperBound iv == hi
prop_boundsCO lo hi = let iv = IntervalCO lo hi in lowerBound iv == lo && upperBound iv == hi
prop_boundsOC lo hi = let iv = IntervalOC lo hi in lowerBound iv == lo && upperBound iv == hi

prop_empty = 
  isEmpty (OpenInterval 1 1) &&
  isEmpty (IntervalCO 1 1) &&
  isEmpty (IntervalOC 1 1) &&
  not (isEmpty (ClosedInterval 1 1))

prop_ord =
  ClosedInterval 1 2 < OpenInterval 1 2 &&
  ClosedInterval 2 3 > OpenInterval 1 2
  

contains :: Ord a => Interval a -> a -> Bool
contains = flip inside

prop_contains1 =
  (c15 `contains` 3) &&
  (o15 `contains` 3) &&
  (co15 `contains` 3) &&
  (oc15 `contains` 3) &&
  (c15 `contains` 1) &&
  (c15 `contains` 5) &&
  not (o15 `contains` 1) &&
  not (o15 `contains` 5) &&
  (co15 `contains` 1) &&
  not (co15 `contains` 5) &&
  not (oc15 `contains` 1) &&
  (oc15 `contains` 5)


prop_overlaps = 
  (c15 `overlaps` ClosedInterval 5 6) &&
  not (c15 `overlaps` OpenInterval 5 6) &&
  (c15 `overlaps` IntervalCO 5 6) &&
  not (c15 `overlaps` IntervalOC 5 6) &&
  (c15 `overlaps` ClosedInterval 0 1) &&
  not (c15 `overlaps` OpenInterval 0 1) &&
  (c15 `overlaps` IntervalOC 0 1) &&
  not (c15 `overlaps` IntervalCO 0 1)

prop_overlapsEmpty =
  not (overlaps (IntervalCO 1 1) (IntervalCO 0 2))

prop_subsumes1 =  (c15 `subsumes` o15) && -- closed subsumes open
                  not (o15 `subsumes` c15) && -- ~? "open does not subsume closed",
                  not (co15 `subsumes` c15) && -- "open does not subsume closed",
                  not (oc15 `subsumes` c15) -- "open does not subsume closed"

ivGen :: Int -> Int -> Gen II
ivGen lo hi = do start <- choose (lo, hi)
                 size  <- choose (0, hi - start)
                 if size == 0
                  then return (II (ClosedInterval start start))
                  else oneof [
                    return (II (ClosedInterval start (start + size))),
                    return (II (OpenInterval start (start + size))),
                    return (II (IntervalCO start (start + size))),
                    return (II (IntervalOC start (start + size))) ]

newtype II = II (Interval Int) deriving (Show)

instance Arbitrary II where
  arbitrary = do x <- arbitrary
                 fmap II (interval (abs x))

interval x = do
             y <- sized (\n -> choose (x, x + abs n))
             if x == y then return (ClosedInterval x y)
                else oneof [return (ClosedInterval x y),
                            return (OpenInterval x y),
                            return (IntervalCO x y),
                            return (IntervalOC x y)]

-- our generator will never generate empty intervals
prop_not_empty (II iv) = not (isEmpty iv)

prop_leftClosed = leftClosed (ClosedInterval 1 2) &&
                  leftClosed (IntervalCO 1 2) &&
                  not (leftClosed (OpenInterval 1 2)) &&
                  not (leftClosed (IntervalOC 1 2))

prop_rightClosed = rightClosed (ClosedInterval 1 2) &&
                   rightClosed (IntervalOC 1 2) &&
                   not (rightClosed (OpenInterval 1 2)) &&
                   not (rightClosed (IntervalCO 1 2))


prop_overlaps_symmetric (II i1) (II i2) = (i1 `overlaps` i2) == (i2 `overlaps` i1)

prop_compare1 (II i1) (II i2) =
  case compare (lowerBound i1) (lowerBound i2) of
    LT -> i1 < i2
    GT -> i1 > i2
    EQ -> True

prop_compare_openness_closedness_lower_bound (II i1) (II i2) =
  if lowerBound i1 == lowerBound i2 then
    let smaller = minimum [i1, i2]
        leftOpen = not . leftClosed
    in leftClosed smaller || (leftOpen i1 && leftOpen i2)
  else
    lowerBound i1 /= lowerBound i2

prop_compare_openness_closedness_upper_bound (II i1) (II i2) =
  if upperBound i1 == upperBound i2 then
    let bigger = maximumBy compareByUpper [i1, i2]
        rightOpen = not. rightClosed
    in rightClosed bigger || (rightOpen i1 && rightOpen i2)
  else
    upperBound i1 /= upperBound i2

prop_combine_closedness =
  let maybeTest = maybe False
  in maybeTest (c15 ==) (combine co15 oc15) &&
     maybeTest (c15 ==) (combine c15 o15) &&
     maybeTest (o15 ==) (combine o15 o15) &&
     maybeTest (co15 ==) (combine co15 o15) &&
     maybeTest (oc15 ==) (combine oc15 o15)

prop_combine_reflexive (II i) =
  let maybeTest = maybe False
  in maybeTest (i ==) (combine i i)

prop_combine_overlapping (II a) (II b) =
  isJust (combine a b) === (a `overlaps` b)

prop_combine_bounds (II a) (II b) =
  case combine a b of
    Nothing -> True
    Just v -> lowerBound v == min (lowerBound a) (lowerBound b) &&
              upperBound v == max (upperBound a) (upperBound b)

prop_contains (II i) p =
  if p `inside` i then
    lowerBound i <= p && upperBound i >= p
  else
    p <= lowerBound i || p >= upperBound i

prop_subsumes (II i1) = forAll subIv (\(II i2) -> (i1 `subsumes` i2) ==>
                                                    ((i1 == i2) || not (i2 `subsumes` i1)))
  where
    subIv = ivGen (lowerBound i1) (upperBound i1)

prop_equals (II a) (II b) =
  (lowerBound a /= lowerBound b || upperBound a /= upperBound b) ==> (a /= b)

prop_below p (II i) =
  let x = lowerBound i in
  if p `below` i
    then (if leftClosed i then p < x else p <= x)
    else (if leftClosed i then p >= x else p > x)

prop_above p (II i) =
  let u = upperBound i in
  if p `above` i
    then (if rightClosed i then p > u else p >= u)
    else (if rightClosed i then p <= u else p < u)

prop_after (II a) (II b) =
  let u = upperBound b
      l = lowerBound a
  in
  if a `after` b
    then (if rightClosed b && leftClosed a then l > u else l >= u)
    else (if rightClosed b && leftClosed a then l <= u else l < u)

prop_readShow (II i) =            i === read (show i)

check p name = do r <- quickCheckWithResult (stdArgs { maxSuccess = 500 }) p
                  if isSuccess r
                   then return r
                   else do putStrLn ("error: " ++ name ++ ": " ++ show r)
                           exitFailure


main = do
         check prop_boundsO "boundsO"
         check prop_boundsC "boundsC"
         check prop_boundsOC "boundsOC"
         check prop_boundsCO "boundsCO"
         check prop_empty "empty"
         check prop_leftClosed "leftClosed"
         check prop_rightClosed "rightClosed"
         check prop_ord "ord"
         check prop_compare1 "compare1"
	 check prop_compare_openness_closedness_lower_bound "compare_openness_closedness_lower_bound"
	 check prop_compare_openness_closedness_upper_bound "compare_openness_closedness_upper_bound"
	 check prop_contains1 "contains1"
	 check prop_overlaps "overlaps"
	 check prop_overlapsEmpty "empty does not overlap"
	 check prop_subsumes1 "subsumes1"
	 check prop_not_empty "not empty"
         check prop_below "below"
         check prop_above "above"
         check prop_after "after"
	 check prop_overlaps_symmetric "overlaps symmetric"
	 check prop_combine_closedness "combine_closedness"
	 check prop_combine_reflexive "combine_reflexive"
	 check prop_combine_overlapping "combine_overlapping"
	 check prop_combine_bounds "combine_bounds"
	 check prop_contains "contains"
	 check prop_subsumes "subsumes"
	 check prop_equals "equals"
         check prop_readShow "read/show"
	 exitSuccess
