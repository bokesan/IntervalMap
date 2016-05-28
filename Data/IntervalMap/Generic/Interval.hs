-- |
-- Module      :  Data.IntervalMap.Generic.Interval
-- Copyright   :  (c) Christoph Breitkopf 2014
-- License     :  BSD-style
-- Maintainer  :  chbreitkopf@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (MPTC with FD)
--
-- Type class for IntervalMap keys.
--
-- As there is no sensible default, no instances for prelude types
-- are provided (E.g. you might want to have tuples as closed
-- intervals in one case, and open in another).
--
-- Empty intervals, i.e. intervals where 'lowerBound >= upperBound' should be avoided
-- if possible. If you must use empty intervals, you need to provide implementations
-- for all operations, as the default implementations do not necessarily work correctly.
-- for example, the default implementation of 'inside' returns 'True' if the point
-- is equal to the lowerBound of a left-closed interval even if it is larger than
-- the upper bound.

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
-- A minimal instance declaration for a closed interval needs only
-- to define 'lowerBound' and 'upperBound'.
class Ord e => Interval i e | i -> e where
  -- | lower bound
  lowerBound :: i -> e

  -- | upper bound
  upperBound :: i -> e

  -- | Does the interval include its lower bound?
  -- Default is True for all values, i.e. closed intervals.
  leftClosed :: i -> Bool
  leftClosed  _ = True

  -- | Does the interval include its upper bound bound?
  -- Default is True for all values, i.e. closed intervals.
  rightClosed :: i -> Bool
  rightClosed _ = True

  -- | Interval strictly before another?
  -- True if the upper bound of the first interval is below the lower bound of the second.
  before :: i -> i -> Bool
  a `before` b = upperBound a < lowerBound b
                 || (upperBound a == lowerBound b && not (rightClosed a && leftClosed b))

  -- | Interval strictly after another?
  -- Same as 'flip before'.
  after :: i -> i -> Bool
  a `after` b  = b `before` a

  -- | Does the first interval completely contain the second?
  subsumes :: i -> i -> Bool
  a `subsumes` b = (lowerBound a < lowerBound b || (lowerBound a == lowerBound b && (leftClosed a || not (leftClosed b))))
                   &&
                   (upperBound a > upperBound b || (upperBound a == upperBound b && (rightClosed a || not (rightClosed b))))

  -- | Do the two intervals overlap?
  overlaps :: i -> i -> Bool
  a `overlaps` b = (lowerBound a < upperBound b || (lowerBound a == upperBound b && leftClosed a && rightClosed b))
                   &&
                   (upperBound a > lowerBound b || (upperBound a == lowerBound b && rightClosed a && leftClosed b))

  -- | Is a point strictly less than lower bound?
  below :: e -> i -> Bool
  p `below` i = case compare p (lowerBound i) of
                  LT -> True
                  EQ -> not (leftClosed i)
                  GT -> False

  -- | Is a point strictly greater than upper bound?
  above :: e -> i -> Bool
  p `above` i = case compare p (upperBound i) of
                  LT -> False
                  EQ -> not (rightClosed i)
                  GT -> True

  -- | Does the interval contain a given point?
  inside :: e -> i -> Bool
  p `inside` i = not ((p `above` i) || (p `below` i)) 

  -- | Is the interval empty?
  isEmpty :: i -> Bool
  isEmpty i | leftClosed i && rightClosed i = lowerBound i >  upperBound i
            | otherwise                     = lowerBound i >= upperBound i

{-
-- sample instance for tuples:
instance Ord e => Interval (e,e) e where
  lowerBound (a,_) = a
  upperBound (_,b) = b
-}

genericEquals :: (Interval i e) => i -> i -> Bool
genericEquals a b = lowerBound a == lowerBound b && upperBound a == upperBound b
                    && leftClosed a == leftClosed b
                    && rightClosed a == rightClosed b

genericCompare :: (Interval i e) => i -> i -> Ordering
genericCompare a b = case compareL a b of
                       LT -> LT
                       GT -> GT
                       EQ -> compareU a b

compareL :: (Interval i e) => i -> i -> Ordering
compareL a b = case compare (lowerBound a) (lowerBound b) of
                 LT -> LT
                 GT -> GT
                 EQ -> case (leftClosed a, leftClosed b) of
                         (True, False) -> LT
                         (False, True) -> GT
                         _ -> EQ

compareU :: (Interval i e) => i -> i -> Ordering
compareU a b = case compare (upperBound a) (upperBound b) of
                 LT -> LT
                 GT -> GT
                 EQ -> case (rightClosed a, rightClosed b) of
                         (True, False) -> GT
                         (False, True) -> LT
                         _ -> EQ

instance Ord a => Interval (I.Interval a) a where
    lowerBound  = I.lowerBound
    upperBound  = I.upperBound
    leftClosed  = I.leftClosed
    rightClosed = I.rightClosed
    overlaps    = I.overlaps
    subsumes    = I.subsumes
    before      = I.before
    after       = I.after
    above       = I.above
    below       = I.below
    inside      = I.inside
    isEmpty     = I.isEmpty
