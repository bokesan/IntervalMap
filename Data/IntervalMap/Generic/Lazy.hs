{- |
Module      :  Data.IntervalMap.Generic.Lazy
Copyright   :  (c) Christoph Breitkopf 2014
License     :  BSD-style
Maintainer  :  chbreitkopf@gmail.com
Stability   :  experimental
Portability :  non-portable (MPTC with FD)

An implementation of maps from intervals to values. The key intervals may
overlap, and the implementation contains efficient search functions
for all keys containing a point or overlapping an interval.
Closed, open, and half-open intervals can be contained in the same map.

This module implements the same functions as "Data.IntervalMap.Generic.Strict",
but with value-lazy semantics.
-}
module Data.IntervalMap.Generic.Lazy (
            -- * re-export
            Interval(..)
            -- * Map type
            , IntervalMap      -- instance Eq,Show,Read

            -- * Operators
            , (!), (\\)

            -- * Query
            , M.null
            , size
            , member
            , notMember
            , M.lookup
            , findWithDefault

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
            , M.map
            , mapWithKey
            , mapAccum
            , mapAccumWithKey
            , mapAccumRWithKey
            , mapKeys
            , mapKeysWith
            , mapKeysMonotonic

            -- ** Fold
            , M.foldr, M.foldl
            , foldrWithKey, foldlWithKey
            , flattenWith, flattenWithMonotonic

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
            , M.filter
            , filterWithKey
            , partition
            , partitionWithKey

            , mapMaybe
            , mapMaybeWithKey
            , mapEither
            , mapEitherWithKey

            , split
            , splitLookup

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

import Data.IntervalMap.Generic.Base as M
