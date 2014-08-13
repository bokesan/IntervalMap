{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- module IntervalTests (main) where

import System.Exit (exitSuccess, exitFailure)

import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)
import Control.Monad (liftM)

import Data.IntervalMap.Generic.Interval

data IV a = IV !Bool !a !a !Bool deriving (Eq, Show)

-- Note that we can not derive Ord for intervals with "mixed" closedness.
-- If, in contrast, all values have identical closedness, deriving should work.
instance Ord a => Ord (IV a) where
  compare = genericCompare

instance Ord a => Interval (IV a) a where
  lowerBound (IV _ x _ _) = x
  upperBound (IV _ _ x _) = x
  leftClosed (IV c _ _ _) = c
  rightClosed (IV _ _ _ c) = c

closedInterval, openInterval, intervalCO, intervalOC :: a -> a -> IV a
closedInterval a b = IV True a b True
openInterval a b = IV False a b False
intervalCO a b = IV True a b False
intervalOC a b = IV False a b True


c15, o15, co15, oc15 :: IV Int
c15 = closedInterval 1 5
o15 = openInterval 1 5
co15 = intervalCO 1 5
oc15 = intervalOC 1 5


prop_boundsC, prop_boundsO, prop_boundsCO, prop_boundsOC :: Int -> Int -> Bool
prop_boundsC lo hi = let iv = closedInterval lo hi in lowerBound iv == lo && upperBound iv == hi
prop_boundsO lo hi = let iv = openInterval lo hi in lowerBound iv == lo && upperBound iv == hi
prop_boundsCO lo hi = let iv = intervalCO lo hi in lowerBound iv == lo && upperBound iv == hi
prop_boundsOC lo hi = let iv = intervalOC lo hi in lowerBound iv == lo && upperBound iv == hi

prop_empty = 
  isEmpty (openInterval 1 1) &&
  isEmpty (intervalCO 1 1) &&
  isEmpty (intervalOC 1 1) &&
  not (isEmpty (closedInterval 1 1))

prop_ord =
  closedInterval 1 2 < openInterval 1 2 &&
  closedInterval 2 3 > openInterval 1 2
  

contains :: Interval i e => i -> e -> Bool
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
  (c15 `overlaps` closedInterval 5 6) &&
  not (c15 `overlaps` openInterval 5 6) &&
  (c15 `overlaps` intervalCO 5 6) &&
  not (c15 `overlaps` intervalOC 5 6) &&
  (c15 `overlaps` closedInterval 0 1) &&
  not (c15 `overlaps` openInterval 0 1) &&
  (c15 `overlaps` intervalOC 0 1) &&
  not (c15 `overlaps` intervalCO 0 1)

prop_subsumes1 =  (c15 `subsumes` o15) && -- closed subsumes open
		  not (o15 `subsumes` c15) && -- ~? "open does not subsume closed",
		  not (co15 `subsumes` c15) && -- "open does not subsume closed",
		  not (oc15 `subsumes` c15) -- "open does not subsume closed"

ivGen :: Int -> Int -> Gen II
ivGen lo hi = do start <- choose (lo, hi)
		 size  <- choose (0, hi - start)
		 if size == 0
		  then return (II (closedInterval start start))
		  else oneof [
		    return (II (closedInterval start (start + size))),
		    return (II (openInterval start (start + size))),
		    return (II (intervalCO start (start + size))),
		    return (II (intervalOC start (start + size))) ]

newtype II = II (IV Int) deriving (Show)

instance Arbitrary II where
  arbitrary = do x <- arbitrary
		 liftM II (interval (abs x))

interval x = do
	     y <- sized (\n -> choose (x, x + abs n))
	     if x == y then return (closedInterval x y)
		else oneof [return (closedInterval x y),
			    return (openInterval x y),
			    return (intervalCO x y),
			    return (intervalOC x y)]

-- our generator will never generate empty intervals
prop_not_empty (II iv) = not (isEmpty iv)

prop_leftClosed = leftClosed (closedInterval 1 2) &&
                  leftClosed (intervalCO 1 2) &&
                  not (leftClosed (openInterval 1 2)) &&
		  not (leftClosed (intervalOC 1 2))

prop_rightClosed = rightClosed (closedInterval 1 2) &&
                   rightClosed (intervalOC 1 2) &&
                   not (rightClosed (openInterval 1 2)) &&
                   not (rightClosed (intervalCO 1 2))


prop_overlaps_symmetric (II i1) (II i2) = (i1 `overlaps` i2) == (i2 `overlaps` i1)

prop_compare1 (II i1) (II i2) =
  case compare (lowerBound i1) (lowerBound i2) of
    LT -> compare i1 i2 == LT
    GT -> compare i1 i2 == GT
    EQ -> True

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
	 check prop_contains1 "contains1"
	 check prop_overlaps "overlaps"
	 check prop_subsumes1 "subsumes1"
	 check prop_not_empty "not empty"
	 check prop_overlaps_symmetric "overlaps symmetric"
	 check prop_contains "contains"
	 check prop_subsumes "subsumes"
	 check prop_equals "equals"
	 exitSuccess
