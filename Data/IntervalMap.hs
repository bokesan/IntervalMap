-- |
-- Module      :  Data.IntervalMap
-- Copyright   :  (c) Christoph Breitkopf 2011
-- License     :  BSD-style
-- Maintainer  :  chbreitkopf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- An implementation of maps from intervals to values. The key intervals may
-- overlap, and the implementation contains efficient search functions
-- for all keys containing a point or overlapping an interval.
-- Closed, open, and half-open intervals can be contained in the same map.
--
-- This module re-exports the value lazy 'Data.IntervalMap.Lazy' API, plus
-- several value strict functions from 'Data.IntervalMap.Strict'.
--
-- An IntervalMap cannot contain duplicate keys - if you need to map a key
-- to multiple values, use a collection as the value type, for
-- example: @IntervalMap /k/ [/v/]@.
--
-- It is an error to insert an empty interval into a map. This precondition is not
-- checked by the various construction functions.
--
-- Since many function names (but not the type name) clash with
-- /Prelude/ names, this module is usually imported @qualified@, e.g.
--
-- >  import Data.IntervalMap (IvMap)
-- >  import qualified Data.IntervalMap as IvMap
--
-- It offers most of the same functions as 'Data.Map', but uses 'Interval' /k/ instead of
-- just /k/ as the key type. Some of the functions need stricter type constraints to
-- maintain the additional information for efficient interval searching,
-- for example 'fromDistinctAscList' needs an 'Ord' /k/ constraint.
-- Also, some functions differ in asymptotic performance (for example 'size') or have not
-- been tuned for efficiency as much as their equivalents in 'Data.Map' (in
-- particular the various set functions).
--
-- In addition, there are functions specific to maps of intervals, for example to search
-- for all keys containing a given point or contained in a given interval.
--
-- To stay compatible with standard Haskell, this implementation uses a fixed data
-- type for intervals, and not a multi-parameter type class. Thus, it's currently
-- not possible to define e.g. a 2-tuple as an instance of interval and use that
-- map key. Instead, you must convert your keys to 'Interval'.
--
-- The implementation is a red-black tree augmented with the maximum upper bound
-- of all keys.
--
-- Parts of this implementation are based on code from the 'Data.Map' implementation,
-- (c) Daan Leijen 2002, (c) Andriy Palamarchuk 2008.
-- The red-black tree deletion is based on code from llrbtree by Kazu Yamamoto.
-- Of course, any errors are mine.
--
module Data.IntervalMap
    ( module Data.IntervalMap.Lazy
    , insertWith'
    , insertWithKey'
    , insertLookupWithKey'
    , fold
    , foldWithKey
    ) where

import Data.IntervalMap.Lazy
import qualified Data.IntervalMap.Lazy as L
import qualified Data.IntervalMap.Strict as S

-- | /Deprecated./ As of version 0.3, replaced by 'S.insertWith'.
--
-- /O(log n)/. Same as 'insertWith', but the combining function is
-- applied strictly.  This is often the most desirable behavior.
--
-- For example, to update a counter:
--
-- > insertWith' (+) k 1 m
--
insertWith' :: Ord k => (a -> a -> a) -> Interval k -> a -> IntervalMap k a -> IntervalMap k a
insertWith' = S.insertWith
{-# INLINABLE insertWith' #-}

-- | /Deprecated./ As of version 0.3, replaced by 'S.insertWithKey'.
--
-- /O(log n)/. Same as 'insertWithKey', but the combining function is
-- applied strictly.
insertWithKey' :: Ord k => (Interval k -> a -> a -> a) -> Interval k -> a -> IntervalMap k a -> IntervalMap k a
insertWithKey' = S.insertWithKey
{-# INLINABLE insertWithKey' #-}

-- | /Deprecated./ As of version 0.3, replaced by
-- 'S.insertLookupWithKey'.
--
-- /O(log n)/. A strict version of 'insertLookupWithKey'.
insertLookupWithKey' :: Ord k => (Interval k -> a -> a -> a) -> Interval k -> a -> IntervalMap k a
                     -> (Maybe a, IntervalMap k a)
insertLookupWithKey' = S.insertLookupWithKey

-- | /Deprecated./ As of version 0.5, replaced by 'L.foldr'.
--
-- /O(n)/. Fold the values in the map using the given right-associative
-- binary operator. This function is an equivalent of 'foldr' and is present
-- for compatibility only.
fold :: (a -> b -> b) -> b -> IntervalMap k a -> b
fold = L.foldr

-- | /Deprecated./ As of version 0.3, replaced by 'L.foldrWithKey'.
--
-- /O(n)/. Fold the keys and values in the map using the given right-associative
-- binary operator. This function is an equivalent of 'foldrWithKey' and is present
-- for compatibility only.
foldWithKey :: (Interval k -> a -> b -> b) -> b -> IntervalMap k a -> b
foldWithKey = foldrWithKey
