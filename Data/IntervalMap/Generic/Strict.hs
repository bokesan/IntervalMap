{- |
Module      :  Data.IntervalMap.Generic.Strict
Copyright   :  (c) Christoph Breitkopf 2014
License     :  BSD-style
Maintainer  :  chbreitkopf@gmail.com
Stability   :  experimental
Portability :  non-portable (MPTC with FD)

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

>  import Data.Generic.IntervalMap.Strict (IntervalMap)
>  import qualified Data.Generic.IntervalMap.Strict as IM

It offers most of the same functions as 'Data.Map', but the
key type must be an instance of 'Interval'.
Some functions differ in asymptotic performance (for example 'size') or
have not been tuned for efficiency as much as their equivalents in
'Data.Map' (in particular the various set functions).

In addition, there are functions specific to maps of intervals, for
example to search for all keys containing a given point or contained
in a given interval.

The implementation is a red-black tree augmented with the maximum
upper bound of all keys.

Parts of this implementation are based on code from the 'Data.Map'
implementation, (c) Daan Leijen 2002, (c) Andriy Palamarchuk 2008. The
red-black tree deletion is based on code from llrbtree by Kazu
Yamamoto. Of course, any errors are mine.
-}
module Data.IntervalMap.Generic.Strict (
            -- * re-export
            Interval(..)
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

import Prelude hiding (null, lookup, map, filter, foldr, foldl, splitAt)
import qualified Data.List as L
import Data.Maybe
import Data.IntervalMap.Generic.Base as M hiding (
      singleton
    , insert
    , insertWith
    , insertWithKey
    , findWithDefault
    , insertLookupWithKey
    , adjust
    , adjustWithKey
    , update
    , updateWithKey
    , updateLookupWithKey
    , alter
    , unionWith
    , unionWithKey
    , unionsWith
    , differenceWith
    , differenceWithKey
    , intersectionWith
    , intersectionWithKey
    , map
    , mapWithKey
    , mapAccum
    , mapAccumWithKey
    , mapAccumRWithKey
    , mapKeysWith
    , fromList
    , fromListWith
    , fromListWithKey
    , fromAscList
    , fromAscListWith
    , fromAscListWithKey
    , mapMaybe
    , mapMaybeWithKey
    , mapEither
    , mapEitherWithKey
    , updateMin
    , updateMax
    , updateMinWithKey
    , updateMaxWithKey
  )

-- | /O(1)/. A map with one entry.
singleton :: k -> v -> IntervalMap k v
singleton k v = v `seq` Node B k k v Nil Nil


-- | /O(log n)/. The expression @('findWithDefault' def k map)@ returns
-- the value at key @k@ or returns default value @def@
-- when the key is not in the map.
--
-- > findWithDefault 'x' 1 (fromList [(5,'a'), (3,'b')]) == 'x'
-- > findWithDefault 'x' 5 (fromList [(5,'a'), (3,'b')]) == 'a'
findWithDefault :: Ord k => a -> k -> IntervalMap k a -> a
findWithDefault def k m = def `seq` fromMaybe def (M.lookup k m)

-- | /O(log n)/. Insert a new key/value pair. If the map already contains the key, its value is
-- changed to the new value.
insert :: (Interval k e, Ord k) => k -> v -> IntervalMap k v -> IntervalMap k v
insert =  insertWithKey (\_ v _ -> v)

-- | /O(log n)/. Insert with a function, combining new value and old value.
-- @'insertWith' f key value mp@ 
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert the pair @(key, f new_value old_value)@.
insertWith :: (Interval k e, Ord k) => (v -> v -> v) -> k -> v -> IntervalMap k v -> IntervalMap k v
insertWith f = insertWithKey (\_ new old -> f new old)

-- | /O(log n)/. Insert with a function, combining key, new value and old value.
-- @'insertWithKey' f key value mp@ 
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert the pair @(key, f key new_value old_value)@.
-- Note that the key passed to f is the same key passed to 'insertWithKey'.
insertWithKey :: (Interval k e, Ord k) => (k -> v -> v -> v) -> k -> v -> IntervalMap k v -> IntervalMap k v
insertWithKey f key value mp  =  key `seq` turnBlack (ins mp)
  where
    singletonR k v = Node R k k v Nil Nil
    ins Nil = value `seq` singletonR key value
    ins (Node color k m v l r) =
      case compare key k of
        LT -> balanceL color k v (ins l) r
        GT -> balanceR color k v l (ins r)
        EQ -> let v' = f k value v in v' `seq` Node color k m v' l r


-- | /O(log n)/. Combine insert with old values retrieval.
insertLookupWithKey :: (Interval k e, Ord k) => (k -> v -> v -> v) -> k -> v -> IntervalMap k v -> (Maybe v, IntervalMap k v)
insertLookupWithKey f key value mp  =  key `seq` (oldval, turnBlack mp')
  where
    (oldval, mp') = ins mp
    singletonR k v = Node R k k v Nil Nil
    ins Nil = value `seq` (Nothing, singletonR key value)
    ins (Node color k m v l r) =
      case compare key k of
        LT -> case ins l of
                 (x@(Just _), t') -> (x, Node color k m v t' r)
                 (Nothing, t') -> (Nothing, balanceL color k v t' r)
        GT -> case ins r of
                 (x@(Just _), t') -> (x, Node color k m v l t')
                 (Nothing, t') -> (Nothing, balanceR color k v l t')
        EQ -> let v' = f k value v in v' `seq` (Just v, Node color k m v' l r)


-- | /O(log n)/. Update a value at a specific key with the result of the provided function.
-- When the key is not
-- a member of the map, the original map is returned.
adjust :: Ord k => (a -> a) -> k -> IntervalMap k a -> IntervalMap k a
adjust f k m = adjustWithKey (\_ v -> f v) k m

-- | /O(log n)/. Adjust a value at a specific key. When the key is not
-- a member of the map, the original map is returned.
adjustWithKey :: Ord k => (k -> a -> a) -> k -> IntervalMap k a -> IntervalMap k a
adjustWithKey _ _ Nil = Nil
adjustWithKey f x (Node c k m v l r) =
  case compare x k of
    LT -> Node c k m v (adjustWithKey f x l) r
    GT -> Node c k m v l (adjustWithKey f x r)
    EQ -> let v' = f k v in v' `seq` Node c k m v' l r

-- | /O(log n)/. Update or delete value at minimum key.
updateMin :: (Interval k e, Ord k) => (v -> Maybe v) -> IntervalMap k v -> IntervalMap k v
updateMin f m = updateMinWithKey (\_ v -> f v) m

-- | /O(log n)/. Update or delete value at maximum key.
updateMax :: (Interval k e, Ord k) => (v -> Maybe v) -> IntervalMap k v -> IntervalMap k v
updateMax f m = updateMaxWithKey (\_ v -> f v) m

-- | /O(log n)/. Update or delete value at minimum key.
updateMinWithKey :: (Interval k e, Ord k) => (k -> v -> Maybe v) -> IntervalMap k v -> IntervalMap k v
updateMinWithKey _ Nil = Nil
updateMinWithKey f m = let (k,v) = findMin m in
                       case f k v of
                         Just v' -> v' `seq` setMinValue v' m
                         Nothing -> deleteMin m

-- | /O(log n)/. Update or delete value at maximum key.
updateMaxWithKey :: (Interval k e, Ord k) => (k -> v -> Maybe v) -> IntervalMap k v -> IntervalMap k v
updateMaxWithKey _ Nil = Nil
updateMaxWithKey f m = let (k,v) = findMax m in
                       case f k v of
                         Just v' -> v' `seq` setMaxValue v' m
                         Nothing -> deleteMax m

-- | /O(n log n)/. Build a map from a list of key\/value pairs. See also 'fromAscList'.
-- If the list contains more than one value for the same key, the last value
-- for the key is retained.
fromList :: (Interval k e, Ord k) => [(k,v)] -> IntervalMap k v
fromList xs = L.foldl' (\m (k,v) -> insert k v m) empty xs

-- | /O(n log n)/. Build a map from a list of key\/value pairs with a combining function. See also 'fromAscListWith'.
fromListWith :: (Interval k e, Ord k) => (a -> a -> a) -> [(k,a)] -> IntervalMap k a
fromListWith f xs = fromListWithKey (\_ x y -> f x y) xs

-- | /O(n log n)/. Build a map from a list of key\/value pairs with a combining function. See also 'fromAscListWith'.
fromListWithKey :: (Interval k e, Ord k) => (k -> a -> a -> a) -> [(k,a)] -> IntervalMap k a
fromListWithKey f xs = L.foldl' ins empty xs
  where
    ins t (k,x) = insertWithKey f k x t

-- | /O(n)/. Build a map from an ascending list in linear time.
-- /The precondition (input list is ascending) is not checked./
fromAscList :: (Interval k e, Eq k) => [(k,v)] -> IntervalMap k v
fromAscList xs = fromAscListWith (\_ b -> b) xs

-- | /O(n)/. Build a map from an ascending list in linear time with a combining function for equal keys.
-- /The precondition (input list is ascending) is not checked./
fromAscListWith :: (Interval k e, Eq k) => (a -> a -> a) -> [(k,a)] -> IntervalMap k a 
fromAscListWith f xs = fromAscListWithKey (\_ a b -> f a b) xs

-- | /O(n)/. Build a map from an ascending list in linear time with a combining function for equal keys.
-- /The precondition (input list is ascending) is not checked./
fromAscListWithKey :: (Interval k e, Eq k) => (k -> a -> a -> a) -> [(k,a)] -> IntervalMap k a
fromAscListWithKey f xs = fromDistinctAscList (combineEq f xs)

combineEq :: Eq k => (k -> a -> a -> a) -> [(k,a)] -> [(k,a)]
combineEq _ [] = []
combineEq _ xs@[_] = xs
combineEq f (x@(xk,xv) : xs@((yk,yv) : xs'))
  | xk == yk  = let v' = f xk xv yv in v' `seq` combineEq f ((xk, v') : xs')
  | otherwise = x : combineEq f xs


-- | /O(n)/. Map a function over all values in the map.
map :: (a -> b) -> IntervalMap k a -> IntervalMap k b
map f = mapWithKey (\_ x -> f x)

-- | /O(n)/. Map a function over all values in the map.
mapWithKey :: (k -> a -> b) -> IntervalMap k a -> IntervalMap k b
mapWithKey f = go
  where
    go Nil = Nil
    go (Node c k m v l r) = let v' = f k v in v' `seq` Node c k m v' (go l) (go r)

-- | /O(n)/. The function 'mapAccum' threads an accumulating
-- argument through the map in ascending order of keys.
--
-- > let f a b = (a ++ b, b ++ "X")
-- > mapAccum f "Everything: " (fromList [(5,"a"), (3,"b")]) == ("Everything: ba", fromList [(3, "bX"), (5, "aX")])
mapAccum :: (a -> b -> (a,c)) -> a -> IntervalMap k b -> (a, IntervalMap k c)
mapAccum f a m = mapAccumWithKey (\a' _ x' -> f a' x') a m

-- | /O(n)/. The function 'mapAccumWithKey' threads an accumulating
-- argument through the map in ascending order of keys.
--
-- > let f a k b = (a ++ " " ++ (show k) ++ "-" ++ b, b ++ "X")
-- > mapAccumWithKey f "Everything:" (fromList [(5,"a"), (3,"b")]) == ("Everything: 3-b 5-a", fromList [(3, "bX"), (5, "aX")])
mapAccumWithKey :: (a -> k -> b -> (a,c)) -> a -> IntervalMap k b -> (a, IntervalMap k c)
mapAccumWithKey f = go
  where
    go a Nil               = (a,Nil)
    go a (Node c kx m x l r) =
                 let (a1,l') = go a l
                     (a2,x') = f a1 kx x
                     (a3,r') = go a2 r
                 in x' `seq` (a3, Node c kx m x' l' r')

-- | /O(n)/. The function 'mapAccumRWithKey' threads an accumulating
-- argument through the map in descending order of keys.
mapAccumRWithKey :: (a -> k -> b -> (a,c)) -> a -> IntervalMap k b -> (a, IntervalMap k c)
mapAccumRWithKey f = go
  where
    go a Nil = (a, Nil)
    go a (Node c kx m x l r) =
                 let (a1,r') = go a r
                     (a2,x') = f a1 kx x
                     (a3,l') = go a2 l
                 in x' `seq` (a3, Node c kx m x' l' r')


-- | /O(n)/. Map values and collect the 'Just' results.
mapMaybe :: (Interval k e) => (a -> Maybe b) -> IntervalMap k a -> IntervalMap k b
mapMaybe f m = mapMaybeWithKey (\_ v -> f v) m

-- | /O(n)/. Map keys\/values and collect the 'Just' results.
mapMaybeWithKey :: (Interval k e) => (k -> a -> Maybe b) -> IntervalMap k a -> IntervalMap k b
mapMaybeWithKey f m = fromDistinctAscList (mapf [] m)
  where
    mapf z Nil = z
    mapf z (Node _ k _ v l r) = mapf (f' k v z r) l
    f' k v z r = case f k v of
                   Nothing -> mapf z r
                   Just v' -> v' `seq` (k,v') : mapf z r

-- | /O(n)/. Map values and separate the 'Left' and 'Right' results.
mapEither :: (Interval k e) => (a -> Either b c) -> IntervalMap k a -> (IntervalMap k b, IntervalMap k c)
mapEither f m = mapEitherWithKey (\_ v -> f v) m

-- | /O(n)/. Map keys\/values and separate the 'Left' and 'Right' results.
mapEitherWithKey :: (Interval k e) => (k -> a -> Either b c) -> IntervalMap k a -> (IntervalMap k b, IntervalMap k c)
mapEitherWithKey f m = (fromDistinctAscList l, fromDistinctAscList r)
  where
    (l, r) = part [] [] (toDescList m)
    part ls rs [] = (ls, rs)
    part ls rs ((k,v):xs) = case f k v of
                              Left v'  -> v' `seq` part ((k,v'):ls) rs xs
                              Right v' -> v' `seq` part ls ((k,v'):rs) xs


-- | /O(log n)/. The expression (@'alter' f k map@) alters the value @x@ at @k@, or absence thereof.
-- 'alter' can be used to insert, delete, or update a value in a 'Map'.
-- In short : @'lookup' k ('alter' f k m) = f ('lookup' k m)@.
alter :: (Interval k e, Ord k) => (Maybe a -> Maybe a) -> k -> IntervalMap k a -> IntervalMap k a
alter f x m = case lookup x m of
                Nothing -> case f Nothing of
                             Nothing -> m
                             Just v -> insert x v m
                y       -> case f y of
                             Nothing -> delete x m
                             Just v' -> adjust (const v') x m


-- | /O(n log n)/. @'mapKeysWith' c f s@ is the map obtained by applying @f@ to each key of @s@.
-- 
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the associated values will be
-- combined using @c@.
mapKeysWith :: (Interval k2 e, Ord k2) => (a -> a -> a) -> (k1 -> k2) -> IntervalMap k1 a -> IntervalMap k2 a
mapKeysWith c f m = fromListWith c [ (f k, v) | (k, v) <- toAscList m ]

-- | /O(log n)/. The expression (@'update' f k map@) updates the value @x@
-- at @k@ (if it is in the map). If (@f x@) is 'Nothing', the element is
-- deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
update :: (Interval k e, Ord k) => (a -> Maybe a) -> k -> IntervalMap k a -> IntervalMap k a
update f k m = updateWithKey (\_ v -> f v) k m

-- | /O(log n)/. The expression (@'updateWithKey' f k map@) updates the
-- value @x@ at @k@ (if it is in the map). If (@f k x@) is 'Nothing',
-- the element is deleted. If it is (@'Just' y@), the key @k@ is bound
-- to the new value @y@.
updateWithKey :: (Interval k e, Ord k) => (k -> a -> Maybe a) -> k -> IntervalMap k a -> IntervalMap k a
updateWithKey f k m = snd (updateLookupWithKey f k m)

-- | /O(log n)/. Lookup and update. See also 'updateWithKey'.
-- The function returns changed value, if it is updated.
-- Returns the original key value if the map entry is deleted.
updateLookupWithKey :: (Interval k e, Ord k) => (k -> a -> Maybe a) -> k -> IntervalMap k a -> (Maybe a, IntervalMap k a)
updateLookupWithKey f x m = case lookup x m of
                              Nothing -> (Nothing, m)
                              r@(Just v) -> case f x v of
                                              Nothing -> (r, delete x m)
                                              r'@(Just v') -> (r', adjust (const v') x m)


-- | /O(n+m)/. Union with a combining function.
unionWith :: (Interval k e, Ord k) => (a -> a -> a) -> IntervalMap k a -> IntervalMap k a -> IntervalMap k a
unionWith f m1 m2 = unionWithKey (\_ v1 v2 -> f v1 v2) m1 m2

-- | /O(n+m)/. Union with a combining function.
unionWithKey :: (Interval k e, Ord k) => (k -> a -> a -> a) -> IntervalMap k a -> IntervalMap k a -> IntervalMap k a
unionWithKey f m1 m2 = fromDistinctAscList (ascListUnion f (toAscList m1) (toAscList m2))

-- | The union of a list of maps, with a combining operation:
--   (@'unionsWith' f == 'Prelude.foldl' ('unionWith' f) 'empty'@).
unionsWith :: (Interval k e, Ord k) => (a -> a -> a) -> [IntervalMap k a] -> IntervalMap k a
unionsWith f = L.foldl (unionWith f) empty

-- | /O(n+m)/. Difference with a combining function. 
-- When two equal keys are
-- encountered, the combining function is applied to the values of these keys.
-- If it returns 'Nothing', the element is discarded (proper set difference). If
-- it returns (@'Just' y@), the element is updated with a new value @y@. 
differenceWith :: (Interval k e, Ord k) => (a -> b -> Maybe a) -> IntervalMap k a -> IntervalMap k b -> IntervalMap k a
differenceWith f m1 m2 = differenceWithKey (\_ v1 v2 -> f v1 v2) m1 m2

-- | /O(n+m)/. Difference with a combining function. When two equal keys are
-- encountered, the combining function is applied to the key and both values.
-- If it returns 'Nothing', the element is discarded (proper set difference). If
-- it returns (@'Just' y@), the element is updated with a new value @y@. 
differenceWithKey :: (Interval k e, Ord k) => (k -> a -> b -> Maybe a) -> IntervalMap k a -> IntervalMap k b -> IntervalMap k a
differenceWithKey f m1 m2 = fromDistinctAscList (ascListDifference f (toAscList m1) (toAscList m2))

-- | /O(n+m)/. Intersection with a combining function.
intersectionWith :: (Interval k e, Ord k) => (a -> b -> c) -> IntervalMap k a -> IntervalMap k b -> IntervalMap k c
intersectionWith f m1 m2 = intersectionWithKey (\_ v1 v2 -> f v1 v2) m1 m2

-- | /O(n+m)/. Intersection with a combining function.
intersectionWithKey :: (Interval k e, Ord k) => (k -> a -> b -> c) -> IntervalMap k a -> IntervalMap k b -> IntervalMap k c
intersectionWithKey f m1 m2 = fromDistinctAscList (ascListIntersection f (toAscList m1) (toAscList m2))


ascListUnion :: Ord k => (k -> a -> a -> a) -> [(k,a)] -> [(k,a)] -> [(k,a)]
ascListUnion _ [] [] = []
ascListUnion _ [] ys = ys
ascListUnion _ xs [] = xs
ascListUnion f xs@(x@(xk,xv):xs') ys@(y@(yk,yv):ys') =
  case compare xk yk of
    LT -> x : ascListUnion f xs' ys
    GT -> y : ascListUnion f xs ys'
    EQ -> let v' = f xk xv yv in v' `seq` (xk, v') : ascListUnion f xs' ys'

ascListDifference :: Ord k => (k -> a -> b -> Maybe a) -> [(k,a)] -> [(k,b)] -> [(k,a)]
ascListDifference _ [] _  = []
ascListDifference _ xs [] = xs
ascListDifference f xs@(x@(xk,xv):xs') ys@((yk,yv):ys') =
  case compare xk yk of
    LT -> x : ascListDifference f xs' ys
    GT -> ascListDifference f xs ys'
    EQ -> case f xk xv yv of
            Nothing -> ascListDifference f xs' ys'
            Just v' -> v' `seq` (xk,v') : ascListDifference f xs' ys'

ascListIntersection :: Ord k => (k -> a -> b -> c) -> [(k,a)] -> [(k,b)] -> [(k,c)]
ascListIntersection _ [] _ = []
ascListIntersection _ _ [] = []
ascListIntersection f xs@((xk,xv):xs') ys@((yk,yv):ys') =
  case compare xk yk of
    LT -> ascListIntersection f xs' ys
    GT -> ascListIntersection f xs ys'
    EQ -> let v' = f xk xv yv in v' `seq` (xk, v') : ascListIntersection f xs' ys'
