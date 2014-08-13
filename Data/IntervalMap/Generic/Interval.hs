-- |
-- Module      :  Data.IntervalMap.Generic.Interval
-- Copyright   :  (c) Christoph Breitkopf 2014
-- License     :  BSD-style
-- Maintainer  :  chbreitkopf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.IntervalMap.Generic.Interval (
    -- * Interval type
    Interval(..),
    -- * helper functions for declaring Eq and Ord instances
    genericEquals, genericCompare
) where

import qualified Data.IntervalMap.Interval as I


-- | Intervals with endpoints of type @e@.
-- A minimal instance declaration needs to define 'con', 'lowerBound', and 'upperBound'.
class Ord e => Interval i e | i -> e where
  --  Construct interval from endpoints
  -- con :: e -> e -> i e

  -- | lower and upper bound
  lowerBound, upperBound :: i -> e

  -- | Does the interval include its lower bound?
  -- Default is True for all values, i.e. closed intervals.
  leftClosed :: i -> Bool
  leftClosed  _ = True

  -- | Does the interval include its upper bound bound?
  -- Default is True for all values, i.e. closed intervals.
  rightClosed :: i -> Bool
  rightClosed _ = True

  overlaps, subsumes, before, after :: i -> i -> Bool
  a `before` b = upperBound a < lowerBound b
                 || (upperBound a == lowerBound b && not (rightClosed a && leftClosed b))
  a `after` b  = lowerBound a > upperBound b
                 || (lowerBound a == upperBound b && not (leftClosed a && rightClosed b))
  a `subsumes` b = (lowerBound a < lowerBound b || (lowerBound a == lowerBound b && (leftClosed a || not (leftClosed b))))
                   &&
                   (upperBound a > upperBound b || (upperBound a == upperBound b && (rightClosed a || not (rightClosed b))))
  a `overlaps` b = (lowerBound a < upperBound b || (lowerBound a == upperBound b && leftClosed a && rightClosed b))
                   &&
                   (upperBound a > lowerBound b || (upperBound a == lowerBound b && rightClosed a && leftClosed b))

  above, below, inside :: e -> i -> Bool
  p `below` i | leftClosed i  = p <  lowerBound i
              | otherwise     = p <= lowerBound i
  p `above` i | rightClosed i = p >  upperBound i
              | otherwise     = p >= upperBound i
  p `inside` i = not ((p `above` i) || (p `below` i)) 

  isEmpty :: i -> Bool
  isEmpty i | leftClosed i && rightClosed i = lowerBound i >  upperBound i
            | otherwise                     = lowerBound i >= upperBound i

{-
-- sample instance for tuples:
instance Ord e => Interval (e,e) e where
  lowerBound (a,_) = a
  upperBound (_,b) = b
-}

genericEquals :: (Interval i e, Eq e) => i -> i -> Bool
genericEquals a b = lowerBound a == lowerBound b && upperBound a == upperBound b
                    && leftClosed a == leftClosed b
                    && rightClosed a == rightClosed b

genericCompare :: (Interval i e, Ord e) => i -> i -> Ordering
genericCompare a b = case compareL a b of
                       LT -> LT
                       GT -> GT
                       EQ -> compareU a b

compareL :: (Interval i e, Ord e) => i -> i -> Ordering
compareL a b = case compare (lowerBound a) (lowerBound b) of
                 LT -> LT
                 GT -> GT
                 EQ -> case (leftClosed a, leftClosed b) of
                         (True, False) -> LT
                         (False, True) -> GT
                         _ -> EQ

compareU :: (Interval i e, Ord e) => i -> i -> Ordering
compareU a b = case compare (upperBound a) (upperBound b) of
                 LT -> LT
                 GT -> GT
                 EQ -> case (rightClosed a, rightClosed b) of
                         (True, False) -> GT
                         (False, True) -> LT
                         _ -> EQ

instance Ord a => Interval (I.Interval a) a where
  lowerBound = I.lowerBound
  upperBound = I.upperBound
  leftClosed = I.leftClosed
  rightClosed = I.rightClosed
