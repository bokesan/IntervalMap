-- |
-- Module      :  Data.IntervalMap.Interval
-- Copyright   :  (c) Christoph Breitkopf 2011
-- License     :  BSD-style
-- Maintainer  :  chris@chr-breitkopf.de
-- Stability   :  experimental
-- Portability :  portable
--
-- A conservative implementation of Intervals, mostly for use as keys in
-- a 'Data.IntervalMap'.
--
-- This should really be a typeclass, so you could have a tuple be an instance
-- of Interval, but that is currently not possible in standard Haskell.
--
-- The contructor names of the half-open intervals seem somewhat clumsy,
-- and I'm open to suggestions for better names.
--
module Data.IntervalMap.Interval (
    -- * Interval type
    Interval(..),
    -- * Query
    lowerBound, upperBound, leftClosed, rightClosed, isEmpty,
    -- * Interval operations
    overlaps, subsumes, before, after,
    -- * Point operations
    below, inside, above
  ) where

import Control.DeepSeq (NFData(rnf))

-- | Intervals with endpoints of type @a@.
--
-- 'Read' and 'Show' use mathematical notation with square brackets for closed
-- and parens for open intervals.
-- This is better for human readability, but is not a valid Haskell expression.
-- Closed intervals look like a list, open intervals look like a tuple,
-- and half-open intervals look like mismatched parens.
data Interval a = IntervalCO !a !a      -- ^ Including lower bound, excluding upper
                | ClosedInterval !a !a  -- ^ Closed at both ends
                | OpenInterval !a !a    -- ^ Open at both ends
                | IntervalOC !a !a      -- ^ Excluding lower bound, including upper
                  deriving (Eq)

instance Show a => Show (Interval a) where
  showsPrec _ (IntervalCO     a b) = showChar '[' . shows a . showChar ',' . shows b . showChar ')'
  showsPrec _ (ClosedInterval a b) = showChar '[' . shows a . showChar ',' . shows b . showChar ']'
  showsPrec _ (OpenInterval   a b) = showChar '(' . shows a . showChar ',' . shows b . showChar ')'
  showsPrec _ (IntervalOC     a b) = showChar '(' . shows a . showChar ',' . shows b . showChar ']'

instance Read a => Read (Interval a) where
  readsPrec _ = readParen False
                  (\r -> [(ClosedInterval a b, w) | ("[", s) <- lex r,
                                                    (a, t) <- reads s,
                                                    (",", u) <- lex t,
                                                    (b, v) <- reads u,
                                                    ("]", w) <- lex v]
                         ++
                         [(OpenInterval   a b, w) | ("(", s) <- lex r,
                                                    (a, t) <- reads s,
                                                    (",", u) <- lex t,
                                                    (b, v) <- reads u,
                                                    (")", w) <- lex v]
                         ++
                         [(IntervalCO     a b, w) | ("[", s) <- lex r,
                                                    (a, t) <- reads s,
                                                    (",", u) <- lex t,
                                                    (b, v) <- reads u,
                                                    (")", w) <- lex v]
                         ++
                         [(IntervalOC     a b, w) | ("(", s) <- lex r,
                                                    (a, t) <- reads s,
                                                    (",", u) <- lex t,
                                                    (b, v) <- reads u,
                                                    ("]", w) <- lex v]
                      )


instance Ord a => Ord (Interval a) where
  compare (IntervalCO a b)     (IntervalCO c d)     = cmpEeEe a b c d
  compare (IntervalCO a b)     (ClosedInterval c d) = cmpEeOC a b c d
  compare (IntervalCO a _)     (OpenInterval c _)   = cmpCO__ a c
  compare (IntervalCO a _)     (IntervalOC c _)     = cmpCO__ a c
  compare (ClosedInterval a b) (IntervalCO c d)     = cmpEeCO a b c d
  compare (ClosedInterval a b) (ClosedInterval c d) = cmpEeEe a b c d
  compare (ClosedInterval a _) (OpenInterval c _)   = cmpCO__ a c
  compare (ClosedInterval a _) (IntervalOC c _)     = cmpCO__ a c
  compare (OpenInterval a _)   (IntervalCO c _)     = cmpOC__ a c
  compare (OpenInterval a _)   (ClosedInterval c _) = cmpOC__ a c
  compare (OpenInterval a b)   (OpenInterval c d)   = cmpEeEe a b c d
  compare (OpenInterval a b)   (IntervalOC c d)     = cmpEeOC a b c d
  compare (IntervalOC a _)     (IntervalCO c _)     = cmpOC__ a c
  compare (IntervalOC a _)     (ClosedInterval c _) = cmpOC__ a c
  compare (IntervalOC a b)     (OpenInterval c d)   = cmpEeCO a b c d
  compare (IntervalOC a b)     (IntervalOC c d)     = cmpEeEe a b c d

cmpEeEe, cmpEeCO, cmpEeOC :: Ord a => a -> a -> a -> a -> Ordering
cmpEeEe a b c d = case compare a c of
                             EQ -> compare b d
                             r  -> r
cmpEeCO a b c d = case compare a c of
                             EQ -> if b < d then LT else GT
                             r  -> r
cmpEeOC a b c d = case compare a c of
                             EQ -> if b <= d then LT else GT
                             r  -> r
cmpOC__, cmpCO__ :: Ord a => a -> a -> Ordering
cmpOC__ a c     = if a < c then LT else GT
cmpCO__ a c     = if a <= c then LT else GT


instance Functor Interval where
  fmap f (IntervalCO     a b) = IntervalCO     (f a) (f b)
  fmap f (ClosedInterval a b) = ClosedInterval (f a) (f b)
  fmap f (OpenInterval   a b) = OpenInterval   (f a) (f b)
  fmap f (IntervalOC     a b) = IntervalOC     (f a) (f b)

instance NFData a => NFData (Interval a) where
  rnf (IntervalCO     a b) = rnf a `seq` rnf b
  rnf (ClosedInterval a b) = rnf a `seq` rnf b
  rnf (OpenInterval   a b) = rnf a `seq` rnf b
  rnf (IntervalOC     a b) = rnf a `seq` rnf b


-- | Get the lower bound.
lowerBound :: Interval a -> a
lowerBound (ClosedInterval lo _) = lo
lowerBound (OpenInterval lo _) = lo
lowerBound (IntervalCO lo _) = lo
lowerBound (IntervalOC lo _) = lo

-- | Get the upper bound.
upperBound :: Interval a -> a
upperBound (ClosedInterval _ hi) = hi
upperBound (OpenInterval _ hi) = hi
upperBound (IntervalCO _ hi) = hi
upperBound (IntervalOC _ hi) = hi


-- | Is the interval empty?
isEmpty :: (Ord a) => Interval a -> Bool
isEmpty (ClosedInterval a b) = a > b
isEmpty iv = lowerBound iv >= upperBound iv

-- | Does the interval include its lower bound?
leftClosed :: Interval a -> Bool
leftClosed (ClosedInterval _ _) = True
leftClosed (IntervalCO _ _) = True
leftClosed _ = False

-- | Does the interval include its upper bound?
rightClosed :: Interval a -> Bool
rightClosed (ClosedInterval _ _) = True
rightClosed (IntervalOC _ _) = True
rightClosed _ = False


-- | Do the two intervals overlap?
overlaps :: (Ord a) => Interval a -> Interval a -> Bool

overlaps (ClosedInterval lo1 hi1) (ClosedInterval lo2 hi2) =  lo1 <= hi2 && hi1 >= lo2
overlaps (ClosedInterval lo1 hi1) (OpenInterval   lo2 hi2) =  lo1 <  hi2 && hi1 >  lo2
overlaps (ClosedInterval lo1 hi1) (IntervalCO     lo2 hi2) =  lo1 <  hi2 && hi1 >= lo2
overlaps (ClosedInterval lo1 hi1) (IntervalOC     lo2 hi2) =  lo1 <= hi2 && hi1 >  lo2

overlaps (OpenInterval   lo1 hi1) (ClosedInterval lo2 hi2) =  lo1 <  hi2 && hi1 >  lo2
overlaps (OpenInterval   lo1 hi1) (OpenInterval   lo2 hi2) =  lo1 <  hi2 && hi1 >  lo2
overlaps (OpenInterval   lo1 hi1) (IntervalCO     lo2 hi2) =  lo1 <  hi2 && hi1 >  lo2
overlaps (OpenInterval   lo1 hi1) (IntervalOC     lo2 hi2) =  lo1 <  hi2 && hi1 >  lo2

overlaps (IntervalCO     lo1 hi1) (ClosedInterval lo2 hi2) =  lo1 <= hi2 && hi1 >  lo2
overlaps (IntervalCO     lo1 hi1) (OpenInterval   lo2 hi2) =  lo1 <  hi2 && hi1 >  lo2
overlaps (IntervalCO     lo1 hi1) (IntervalCO     lo2 hi2) =  lo1 <  hi2 && hi1 >  lo2
overlaps (IntervalCO     lo1 hi1) (IntervalOC     lo2 hi2) =  lo1 <= hi2 && hi1 >  lo2

overlaps (IntervalOC     lo1 hi1) (ClosedInterval lo2 hi2) =  lo1 <  hi2 && hi1 >= lo2
overlaps (IntervalOC     lo1 hi1) (OpenInterval   lo2 hi2) =  lo1 <  hi2 && hi1 >  lo2
overlaps (IntervalOC     lo1 hi1) (IntervalCO     lo2 hi2) =  lo1 <  hi2 && hi1 >= lo2
overlaps (IntervalOC     lo1 hi1) (IntervalOC     lo2 hi2) =  lo1 <  hi2 && hi1 >  lo2


-- | Does the first interval completely contain the second?
subsumes :: (Ord a) => Interval a -> Interval a -> Bool

subsumes (ClosedInterval lo1 hi1) (ClosedInterval lo2 hi2) =  lo1 <= lo2 && hi1 >= hi2
subsumes (ClosedInterval lo1 hi1) (OpenInterval   lo2 hi2) =  lo1 <= lo2 && hi1 >= hi2
subsumes (ClosedInterval lo1 hi1) (IntervalCO     lo2 hi2) =  lo1 <= lo2 && hi1 >= hi2
subsumes (ClosedInterval lo1 hi1) (IntervalOC     lo2 hi2) =  lo1 <= lo2 && hi1 >= hi2

subsumes (OpenInterval   lo1 hi1) (ClosedInterval lo2 hi2) =  lo1 <  lo2 && hi1 >  hi2
subsumes (OpenInterval   lo1 hi1) (OpenInterval   lo2 hi2) =  lo1 <= lo2 && hi1 >= hi2
subsumes (OpenInterval   lo1 hi1) (IntervalCO     lo2 hi2) =  lo1 <  lo2 && hi1 >= hi2
subsumes (OpenInterval   lo1 hi1) (IntervalOC     lo2 hi2) =  lo1 <= lo2 && hi1 >  hi2

subsumes (IntervalCO     lo1 hi1) (ClosedInterval lo2 hi2) =  lo1 <= lo2 && hi1 >  hi2
subsumes (IntervalCO     lo1 hi1) (OpenInterval   lo2 hi2) =  lo1 <= lo2 && hi1 >= hi2
subsumes (IntervalCO     lo1 hi1) (IntervalCO     lo2 hi2) =  lo1 <= lo2 && hi1 >= hi2
subsumes (IntervalCO     lo1 hi1) (IntervalOC     lo2 hi2) =  lo1 <= lo2 && hi1 >  hi2

subsumes (IntervalOC     lo1 hi1) (ClosedInterval lo2 hi2) =  lo1 <  lo2 && hi1 >= hi2
subsumes (IntervalOC     lo1 hi1) (OpenInterval   lo2 hi2) =  lo1 <= lo2 && hi1 >= hi2
subsumes (IntervalOC     lo1 hi1) (IntervalCO     lo2 hi2) =  lo1 <  lo2 && hi1 >= hi2
subsumes (IntervalOC     lo1 hi1) (IntervalOC     lo2 hi2) =  lo1 <= lo2 && hi1 >= hi2

-- | Interval strictly before another?
-- True if the upper bound of the first interval is below the lower bound of the second.
before :: Ord a => Interval a -> Interval a -> Bool
IntervalCO _ l     `before` r =  l <= lowerBound r
ClosedInterval _ l `before` IntervalCO r _      =  l < r
ClosedInterval _ l `before` ClosedInterval r _  =  l < r
ClosedInterval _ l `before` OpenInterval r _    =  l <= r
ClosedInterval _ l `before` IntervalOC r _      =  l <= r
OpenInterval _ l   `before` r =  l <= lowerBound r
IntervalOC _ l     `before` IntervalCO r _      =  l < r
IntervalOC _ l     `before` ClosedInterval r _  =  l < r
IntervalOC _ l     `before` OpenInterval r _    =  l <= r
IntervalOC _ l     `before` IntervalOC r _      =  l <= r
                                   
-- | Interval strictly after another?
-- Same as 'flip before'.
after :: Ord a => Interval a -> Interval a -> Bool
r `after` l = l `before` r


-- | Does the interval contain a given point?
inside :: (Ord a) => a -> Interval a -> Bool
p `inside` (IntervalCO     lo hi) =  lo <= p && p <  hi
p `inside` (ClosedInterval lo hi) =  lo <= p && p <= hi
p `inside` (OpenInterval   lo hi) =  lo <  p && p <  hi
p `inside` (IntervalOC     lo hi) =  lo <  p && p <= hi

-- | Is a point strictly less than lower bound?
below :: (Ord a) => a -> Interval a -> Bool
p `below` (IntervalCO     l _)  =  p <  l
p `below` (ClosedInterval l _)  =  p <  l
p `below` (OpenInterval   l _)  =  p <= l
p `below` (IntervalOC     l _)  =  p <= l

-- | Is a point strictly greater than upper bound?
above :: (Ord a) => a -> Interval a -> Bool
p `above` (IntervalCO     _ u)  =  p >= u
p `above` (ClosedInterval _ u)  =  p >  u
p `above` (OpenInterval   _ u)  =  p >= u
p `above` (IntervalOC     _ u)  =  p >  u
