{- |
Module      :  Data.IntervalMap.Lazy
Copyright   :  (c) Christoph Breitkopf 2011
License     :  BSD-style
Maintainer  :  chbreitkopf@gmail.com
Stability   :  experimental
Portability :  portable

An implementation of maps from intervals to values. The key intervals may
overlap, and the implementation contains efficient search functions
for all keys containing a point or overlapping an interval.
Closed, open, and half-open intervals can be contained in the same map.

This module implements the same functions as "Data.IntervalMap.Strict",
but with value-lazy semantics.
-}
module Data.IntervalMap.Lazy (
            -- * re-export
            I.Interval(..)
            -- * Map type
            , IntervalMap      -- instance Eq,Show,Read

            -- * Operators
            , (!), (\\)

            -- * Query
            , null
            , size
            , member
            , notMember
            , lookup
            , findWithDefault
            , lookupLT
            , lookupGT
            , lookupLE
            , lookupGE

            -- ** Interval query
            , containing
            , intersecting
            , within
            
            -- * Construction
            , empty
            , singleton

            -- ** Insertion
            , insert
            , insertWith
            , insertWithKey
            , insertLookupWithKey
            
            -- ** Delete\/Update
            , delete
            , adjust
            , adjustWithKey
            , update
            , updateWithKey
            , updateLookupWithKey
            , alter

            -- * Combine

            -- ** Union
            , union
            , unionWith
            , unionWithKey
            , unions
            , unionsWith

            -- ** Difference
            , difference
            , differenceWith
            , differenceWithKey
            
            -- ** Intersection
            , intersection
            , intersectionWith
            , intersectionWithKey

            -- * Traversal
            -- ** Map
            , map
            , mapWithKey
            , mapAccum
            , mapAccumWithKey
            , mapAccumRWithKey
            , mapKeys
            , mapKeysWith
            , mapKeysMonotonic

            -- ** Fold
            , foldr, foldl
            , foldrWithKey, foldlWithKey

            -- * Flatten
            , flattenWith

            -- * Conversion
            , elems
            , keys
            , keysSet
            , assocs

            -- ** Lists
            , toList
            , fromList
            , fromListWith
            , fromListWithKey

            -- ** Ordered lists
            , toAscList
            , toDescList
            , fromAscList
            , fromAscListWith
            , fromAscListWithKey
            , fromDistinctAscList

            -- * Filter
            , filter
            , filterWithKey
            , partition
            , partitionWithKey

            , mapMaybe
            , mapMaybeWithKey
            , mapEither
            , mapEitherWithKey

            , split
            , splitLookup
            , splitAt
            , splitIntersecting

            -- * Submap
            , isSubmapOf, isSubmapOfBy
            , isProperSubmapOf, isProperSubmapOfBy

            -- * Min\/Max
            , findMin
            , findMax
            , findLast
            , deleteMin
            , deleteMax
            , deleteFindMin
            , deleteFindMax
            , updateMin
            , updateMax
            , updateMinWithKey
            , updateMaxWithKey
            , minView
            , maxView
            , minViewWithKey
            , maxViewWithKey

            -- * Debugging
            , valid

            -- * Testing
            , height, maxHeight, showStats

            ) where

import Prelude hiding (filter, foldl, foldr, lookup, map, null, splitAt)
import Data.IntervalMap.Interval as I
import Data.IntervalMap.Generic.Lazy hiding (IntervalMap, flattenWith)
import qualified Data.IntervalMap.Generic.Lazy as M


type IntervalMap k v = M.IntervalMap (I.Interval k) v

-- | /O(n)/. Join overlapping intervals with 'combine'.
--
-- > flattenWith (+) (fromList [([1,3],5), ([4,6],2), ([5,8],9)]) == mkMap [([1,3],5), ([4,8],11)]
flattenWith :: (Ord k) => (v -> v -> v) -> IntervalMap k v -> IntervalMap k v
flattenWith f m = M.flattenWithMonotonic f' m
  where
    f' (k1,v1) (k2,v2) = case combine k1 k2 of
                           Nothing -> Nothing
                           Just k' -> Just (k', f v1 v2)
