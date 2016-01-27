{- |
Module      :  Data.IntervalMap.Strict
Copyright   :  (c) Christoph Breitkopf 2011
License     :  BSD-style
Maintainer  :  chbreitkopf@gmail.com
Stability   :  experimental
Portability :  portable

An implementation of maps from intervals to values. The key intervals
may overlap, and the implementation contains efficient search
functions for all keys containing a point or overlapping an
interval. Closed, open, and half-open intervals can be contained in
the same map.

The functions in this module are strict in both the keys and the
values.  If you need value-lazy maps, use "Data.IntervalMap.Lazy"
instead. The IntervalMap type itself is shared between the lazy and
strict modules, meaning that the same IntervalMap value can be passed
to functions in both modules (although that is rarely needed).

An IntervalMap cannot contain duplicate keys - if you need to map a
key to multiple values, use a collection as the value type, for
example: @IntervalMap /k/ [/v/]@.

It is an error to insert an empty interval into a map. This
precondition is not checked by the various construction functions.

Since many function names (but not the type name) clash with /Prelude/
names, this module is usually imported @qualified@, e.g.

>  import Data.IntervalMap (IvMap)
>  import qualified Data.IntervalMap as IvMap

It offers most of the same functions as 'Data.Map', but uses
'Interval' /k/ instead of just /k/ as the key type. Some of the
functions need stricter type constraints to maintain the additional
information for efficient interval searching, for example
'fromDistinctAscList' needs an 'Ord' /k/ constraint. Also, some
functions differ in asymptotic performance (for example 'size') or
have not been tuned for efficiency as much as their equivalents in
'Data.Map' (in particular the various set functions).

In addition, there are functions specific to maps of intervals, for
example to search for all keys containing a given point or contained
in a given interval.

To stay compatible with standard Haskell, this implementation uses a
fixed data type for intervals, and not a multi-parameter type
class. Thus, it's currently not possible to define e.g. a 2-tuple as
an instance of interval and use that map key. Instead, you must
convert your keys to 'Interval'.

The implementation is a red-black tree augmented with the maximum
upper bound of all keys.

Parts of this implementation are based on code from the 'Data.Map'
implementation, (c) Daan Leijen 2002, (c) Andriy Palamarchuk 2008. The
red-black tree deletion is based on code from llrbtree by Kazu
Yamamoto. Of course, any errors are mine.
-}
module Data.IntervalMap.Strict (
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
            , M.splitAt
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


import Data.IntervalMap.Interval as I
import Data.IntervalMap.Generic.Strict hiding (IntervalMap, null, filter, lookup, map, foldr, foldl, flattenWith)
import qualified Data.IntervalMap.Generic.Strict as M


type IntervalMap k v = M.IntervalMap (I.Interval k) v

-- | /O(n)/. Join overlapping intervals with 'combine'.
--
-- > flattenWith (+) (fromList [([1,3],5), ([4,6],2), ([5,8],9)]) == mkMap [([1,3],5), ([4,8],11)]
flattenWith :: (Ord k) => (v -> v -> v) -> IntervalMap k v -> IntervalMap k v
flattenWith f m = M.flattenWithMonotonic f' m
  where
    f' (k1,v1) (k2,v2) = case combine k1 k2 of
                           Nothing -> Nothing
                           Just k' -> let v' = f v1 v2 in v' `seq` Just (k', v')
