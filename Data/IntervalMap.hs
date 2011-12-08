 -- |
-- Module      :  Data.Map
-- Copyright   :  (c) Christoph Breitkopf 2011
-- License     :  BSD-style
-- Maintainer  :  chris@chr-breitkopf.de
-- Stability   :  experimental
-- Portability :  portable
--
-- An implementation of maps from intervals to values. The key intervals may
-- overlap, and the implementation supports an efficient stabbing query.
--
-- Since many function names (but not the type name) clash with
-- "Prelude" names, this module is usually imported @qualified@, e.g.
--
-- >  import Data.IntervalMap (IvMap)
-- >  import qualified Data.IntervalMap as IvMap
--
-- It offers the same functions as Data.Map, but 'Interval' /k/ instead of
-- just /k/ as the key type. Some of the functions need stricter type constraints to
-- maintain the additional information for efficient interval searching,
-- for example 'fromDistinctAscList' needs an 'Ord' /k/ constraint.
--
-- In addition, there is a function 'searchPoint' for searching all keys that contain
-- a point (stabbing query), and a function 'searchInterval' for all keys overlapping
-- the given interval.
--
-- To stay compatible with standard Haskell, this implementation uses a fixed data
-- type for intervals, and not a multi-parameter type class. Thus, it's currently
-- not possible to define e.g. a 2-tuple as an instance of interval and use that
-- map key. Instead you must convert your keys to Data.Interval.
--
-- Closed, open, and half-open intervals can be contained in the same map.
--
-- It is an error to insert an empty interval into a map. This precondition is not
-- checked by the various insertion functions.
--
-- The implementation is a red-black tree augmented with the maximum upper bound
-- of all keys.
--
-- Parts of this implementation are based on code from the 'Data.Map' implementation,
-- (c) Daan Leijen 2002, (c) Andriy Palamarchuk 2008.
-- The red-black tree deletion is based on code from llrbtree by Kazu Yamamoto.
--
module Data.IntervalMap (
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

            -- ** Interval query
            , searchPoint
            , searchInterval
            
            -- * Construction
            , empty
            , singleton

            -- ** Insertion
            , insert
            , insertWith
            , insertWith'
            , insertWithKey
            , insertWithKey'
            , insertLookupWithKey
            , insertLookupWithKey'
            
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
            , fold
            , foldrWithKey
            , foldlWithKey
            -- , foldlWithKey'

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
            {-

            -- * Submap
            , isSubmapOf, isSubmapOfBy
            , isProperSubmapOf, isProperSubmapOfBy

            -- * Indexed 
            , lookupIndex
            , findIndex
            , elemAt
            , updateAt
            , deleteAt
            -}

            -- * Min\/Max
            , findMin
            , findMax
            , deleteMin
            , deleteMax
            , deleteFindMin
            , deleteFindMax
            {-
            , updateMin
            , updateMax
            , updateMinWithKey
            , updateMaxWithKey
            , minView
            , maxView
            , minViewWithKey
            , maxViewWithKey
            -}

            -- * Debugging
            {-
            , showTree
            , showTreeWith -}
            , valid

            -- * Testing
            , height, maxHeight, showStats

            ) where

import Prelude hiding (null, lookup, map, filter)
import Data.Bits (shiftR, (.&.))
import qualified Data.List as L
import qualified Data.Set as Set
import Data.Interval

{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}
infixl 9 !,\\ --

-- | Lookup value for given key. Calls 'error' if the key is not in the map.
(!) :: (Ord k) => IntervalMap k v -> Interval k -> v
tree ! key = case lookup key tree of
               Just v  -> v
               Nothing -> error "IntervalMap.!: key not found"

-- | Same as 'difference'.
(\\) :: Ord k => IntervalMap k a -> IntervalMap k b -> IntervalMap k a
m1 \\ m2 = difference m1 m2


data Color = R | B deriving (Eq, Show)

-- | A map from intervals with endpoints of type @k@ to values of type @v@.
data IntervalMap k v = Nil
                      | Node !Color
                             !(Interval k) -- key
                             !(UBound k)   -- max upper in tree
                             v             -- value
                             !(IntervalMap k v) -- left subtree
                             !(IntervalMap k v) -- right subtree
                        deriving (Show)

instance (Eq k, Eq v) => Eq (IntervalMap k v) where
  a == b = toAscList a == toAscList b

instance (Ord k, Ord v) => Ord (IntervalMap k v) where
  compare a b = compare (toAscList a) (toAscList b)

instance Functor (IntervalMap k) where
  fmap f m  = map f m


isRed :: IntervalMap k v -> Bool
isRed (Node R _ _ _ _ _) = True
isRed _ = False

-- -------------------------------------------

data UBound a = Excluding !a
              | Including !a
                deriving (Eq)

instance Ord a => Ord (UBound a) where
  compare (Excluding a) (Excluding b) = compare a b
  compare (Excluding a) (Including b) = if a <= b then LT else GT
  compare (Including a) (Including b) = compare a b
  compare (Including a) (Excluding b) = if a < b then LT else GT

instance Show a => Show (UBound a) where
  showsPrec _ (Including x) = showChar '[' . shows x . showChar ']'
  showsPrec _ (Excluding x) = showChar '(' . shows x . showChar ')'


bMax :: Ord a => Interval a -> UBound a -> UBound a
bMax v x@(Excluding b) = case compare (upperBound v) b of
                           LT -> x
                           GT -> uBound v
                           EQ -> if rightClosed v then Including b else x
bMax v x@(Including b) = case compare (upperBound v) b of
                           LT -> x
                           EQ -> x
                           GT -> uBound v


uBound :: Interval a -> UBound a
uBound iv | rightClosed iv = Including (upperBound iv)
          | otherwise      = Excluding (upperBound iv)

greater :: (Ord a) => a -> UBound a -> Bool
a `greater` Including b  =  a > b
a `greater` Excluding b  =  a >= b

less :: (Ord a) => a -> Interval a -> Bool
a `less` iv | leftClosed iv  =  a <  lowerBound iv
            | otherwise      =  a <= lowerBound iv

-- Interval strictly greater than UBound?
vGreater :: (Ord a) => Interval a -> UBound a -> Bool
iv `vGreater` Excluding b  =  lowerBound iv >= b
iv `vGreater` Including b
           | leftClosed iv =  lowerBound iv > b
           | otherwise     =  lowerBound iv >= b

-- Interval strictly below another interval?
vLess :: (Ord a) => Interval a -> Interval a -> Bool
iv1 `vLess` iv2 | rightClosed iv1 && leftClosed iv2 = upperBound iv1 <  lowerBound iv2
                | otherwise                         = upperBound iv1 <= lowerBound iv2


-- | The empty map.
empty :: IntervalMap k v
empty =  Nil

-- | A map with one entry.
singleton :: Interval k -> v -> IntervalMap k v
singleton k v = Node B k (uBound k) v Nil Nil


-- | Is the map empty?
null :: IntervalMap k v -> Bool
null Nil = True
null _   = False

-- | Number of keys in the map.
size :: IntervalMap k v -> Int
size t = h 0 t
  where
    h n Nil = n
    h n (Node _ _ _ _ l r) = let n' = n + size l + 1 in n' `seq` h n' r

-- | The height of the tree. For testing/debugging only.
height :: IntervalMap k v -> Int
height Nil = 0
height (Node _ _ _ _ l r) = 1 + max (height l) (height r)

-- | The maximum height of a red-black tree with the given number of nodes.
maxHeight :: Int -> Int
maxHeight nodes = 2 * log2 (nodes + 1)

-- | Tree statistics (size, height, maxHeight size)
showStats :: IntervalMap k a -> (Int, Int, Int)
showStats m = (n, height m, maxHeight n)
  where n = size m

-- | Does the map contain the given key? See also 'notMember'.
member :: (Ord k) => Interval k -> IntervalMap k v -> Bool
member key tree = case lookup key tree of
                    Nothing -> False
                    Just _  -> True

-- | Does the map not contain the given key? See also 'member'.
notMember :: (Ord k) => Interval k -> IntervalMap k v -> Bool
notMember key tree = not (member key tree)


-- | Look up the given key in the map, returning the value @('Just' value)@,
-- or 'Nothing if the key is not in the map.
lookup :: (Ord k) => Interval k -> IntervalMap k v -> Maybe v
lookup _ Nil  = Nothing
lookup k (Node _ key _ v l r) = case compare k key of
                                  LT -> lookup k l
                                  GT -> lookup k r
                                  EQ -> Just v


-- | /O(log n)/. The expression @('findWithDefault' def k map)@ returns
-- the value at key @k@ or returns default value @def@
-- when the key is not in the map.
--
-- > findWithDefault 'x' 1 (fromList [(5,'a'), (3,'b')]) == 'x'
-- > findWithDefault 'x' 5 (fromList [(5,'a'), (3,'b')]) == 'a'

findWithDefault :: Ord k => a -> Interval k -> IntervalMap k a -> a
findWithDefault def k m = case lookup k m of
    Nothing -> def
    Just x  -> x

-- | Return all key/value pairs where the key intervals contain the given point.
-- The order in which the elements are returned is undefined.
searchPoint :: (Ord k) => k -> IntervalMap k v -> [(Interval k, v)]
searchPoint _ Nil = []
searchPoint p (Node _ k m v l r)
  | p `greater` m   =  []    -- if point is to the right of all intervals in this tree, no result
  | p `less` k      =  searchPoint p l -- if point is to the left of the lower bound, it can't be in the right subtree
  | k `contains` p  =  (k,v) : searchPoint p l ++ searchPoint p r
  | otherwise       =          searchPoint p l ++ searchPoint p r

-- | Return all key/value pairs where the key intervals overlap (intersect) the given interval.
-- The order in which the elements are returned is undefined.
searchInterval :: (Ord k) => Interval k -> IntervalMap k v -> [(Interval k, v)]
searchInterval _ Nil = []
searchInterval i (Node _ k m v l r)
  | i `vGreater` m  =  []
  | i `vLess` k     =  searchInterval i l
  | i `overlaps` k  =  (k,v) : searchInterval i l ++ searchInterval i r
  | otherwise       =          searchInterval i l ++ searchInterval i r

-- | Insert a new key/value pair. If the map already contains the key, its value is
-- changed to the new value.
insert :: (Ord k) => Interval k -> v -> IntervalMap k v -> IntervalMap k v
insert =  insertWithKey' (\_ v _ -> v)

insertWith :: (Ord k) => (v -> v -> v) -> Interval k -> v -> IntervalMap k v -> IntervalMap k v
insertWith f = insertWithKey (\_ new old -> f new old)

insertWith' :: (Ord k) => (v -> v -> v) -> Interval k -> v -> IntervalMap k v -> IntervalMap k v
insertWith' f = insertWithKey' (\_ new old -> f new old)

insertWithKey :: (Ord k) => (Interval k -> v -> v -> v) -> Interval k -> v -> IntervalMap k v -> IntervalMap k v
insertWithKey f k v m =  snd (insertLookupWithKey f k v m)

insertWithKey' :: (Ord k) => (Interval k -> v -> v -> v) -> Interval k -> v -> IntervalMap k v -> IntervalMap k v
insertWithKey' f k v m =  snd (insertLookupWithKey' f k v m)

-- | /O(log n)/. Combine insert with old values retrieval.
insertLookupWithKey :: (Ord k) => (Interval k -> v -> v -> v) -> Interval k -> v -> IntervalMap k v -> (Maybe v, IntervalMap k v)
insertLookupWithKey f key value mp  =  mapSnd makeBlack (ins mp)
  where
    singletonR k v = Node R k (uBound k) v Nil Nil
    ins Nil = (Nothing, singletonR key value)
    ins (Node color k m v l r) =
      case compare key k of
        LT -> case ins l of
                 (x@(Just _), t') -> (x, Node color k m v t' r)
                 (Nothing, t') -> (Nothing, balanceL color k v t' r)
        GT -> case ins r of
                 (x@(Just _), t') -> (x, Node color k m v l t')
                 (Nothing, t') -> (Nothing, balanceR color k v l t')
        EQ -> (Just v, Node color k m (f k value v) l r)

-- | /O(log n)/. Combine insert with old values retrieval.
insertLookupWithKey' :: (Ord k) => (Interval k -> v -> v -> v) -> Interval k -> v -> IntervalMap k v -> (Maybe v, IntervalMap k v)
insertLookupWithKey' f key value mp  =  mapSnd makeBlack (ins mp)
  where
    singletonR k v = Node R k (uBound k) v Nil Nil
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


balanceL :: Ord k => Color -> Interval k -> v -> IntervalMap k v -> IntervalMap k v -> IntervalMap k v
balanceL B zk zv (Node R yk _ yv (Node R xk _ xv a b) c) d =
    mNode R yk yv (mNode B xk xv a b) (mNode B zk zv c d)
balanceL B zk zv (Node R xk _ xv a (Node R yk _ yv b c)) d =
    mNode R yk yv (mNode B xk xv a b) (mNode B zk zv c d)
balanceL c xk xv l r = mNode c xk xv l r

balanceR :: Ord k => Color -> Interval k -> v -> IntervalMap k v -> IntervalMap k v -> IntervalMap k v
balanceR B xk xv a (Node R yk _ yv b (Node R zk _ zv c d)) =
    mNode R yk yv (mNode B xk xv a b) (mNode B zk zv c d)
balanceR B xk xv a (Node R zk _ zv (Node R yk _ yv b c) d) =
    mNode R yk yv (mNode B xk xv a b) (mNode B zk zv c d)
balanceR c xk xv l r = mNode c xk xv l r


mNode :: (Ord k) => Color -> Interval k -> v -> IntervalMap k v -> IntervalMap k v -> IntervalMap k v
mNode c k v l r = Node c k (maxUpper k l r) v l r

maxUpper :: (Ord k) => Interval k -> IntervalMap k v -> IntervalMap k v -> UBound k
maxUpper k Nil                Nil                = uBound k
maxUpper k Nil                (Node _ _ m _ _ _) = bMax k m
maxUpper k (Node _ _ m _ _ _) Nil                = bMax k m
maxUpper k (Node _ _ l _ _ _) (Node _ _ r _ _ _) = bMax k (max l r)


-- min/max

-- | Returns the smallest key and its associated value.
-- Calls 'error' if the map is empty.
findMin :: IntervalMap k v -> (Interval k, v)
findMin (Node _ k _ v Nil _) = (k,v)
findMin (Node _ _ _ _ l _) = findMin l
findMin Nil = error "IntervalMap.findMin: empty map"

-- | Returns the largest key and its associated value.
-- Calls 'error' if the map is empty.
findMax :: IntervalMap k v -> (Interval k, v)
findMax (Node _ k _ v _ Nil) = (k,v)
findMax (Node _ _ _ _ _ r) = findMax r
findMax Nil = error "IntervalMap.findMin: empty map"


-- use our own Either type for readability
data DeleteResult k v = Unchanged (IntervalMap k v)
                      | Shrunk (IntervalMap k v)


-- | Remove the smallest key from the map. Return the empty map if the map is empty.
deleteMin :: (Ord k) => IntervalMap k v -> IntervalMap k v
deleteMin Nil = Nil
deleteMin mp = case deleteMin' mp of
                 (Unchanged r, _, _) -> makeBlack r
                 (Shrunk r, _, _)    -> makeBlack r

deleteMin' :: Ord k => IntervalMap k v -> (DeleteResult k v, Interval k, v)
deleteMin' Nil = error "deleteMin': Nil"
deleteMin' (Node B k _ v Nil Nil) = (Shrunk Nil, k, v)
deleteMin' (Node B k _ v Nil r@(Node R _ _ _ _ _)) = (Unchanged (makeBlack r), k, v)
deleteMin' (Node R k _ v Nil r) = (Unchanged r, k, v)
deleteMin' (Node c k _ v l r) =
  case deleteMin' l of
    (Unchanged l', rk, rv) -> (Unchanged (mNode c k v l' r), rk, rv)
    (Shrunk l',    rk, rv) -> (unbalancedR c k v l' r, rk, rv)

deleteMax' :: Ord k => IntervalMap k v -> (DeleteResult k v, Interval k, v)
deleteMax' Nil = error "deleteMax': Nil"
deleteMax' (Node B k _ v Nil Nil) = (Shrunk Nil, k, v)
deleteMax' (Node B k _ v l@(Node R _ _ _ _ _) Nil) = (Unchanged (makeBlack l), k, v)
deleteMax' (Node R k _ v l Nil) = (Unchanged l, k, v)
deleteMax' (Node c k _ v l r) =
  case deleteMax' r of
    (Unchanged r', rk, rv) -> (Unchanged (mNode c k v l r'), rk, rv)
    (Shrunk    r', rk, rv) -> (unbalancedL c k v l r', rk, rv)

-- The left tree lacks one Black node
unbalancedR :: Ord k => Color -> Interval k -> v -> IntervalMap k v -> IntervalMap k v -> DeleteResult k v
-- Decreasing one Black node in the right
unbalancedR B k v l r@(Node B _ _ _ _ _) = Shrunk    (balanceR B k v l (turnR r))
unbalancedR R k v l r@(Node B _ _ _ _ _) = Unchanged (balanceR B k v l (turnR r))
-- Taking one Red node from the right and adding it to the right as Black
unbalancedR B k v l (Node R rk _ rv rl@(Node B _ _ _ _ _) rr)
  = Unchanged (mNode B rk rv (balanceR B k v l (turnR rl)) rr)
unbalancedR _ _ _ _ _ = error "unbalancedR"

unbalancedL :: Ord k => Color -> Interval k -> v -> IntervalMap k v -> IntervalMap k v -> DeleteResult k v
unbalancedL B k v l@(Node B _ _ _ _ _) r = Shrunk    (balanceL B k v (turnR l) r)
unbalancedL R k v l@(Node B _ _ _ _ _) r = Unchanged (balanceL B k v (turnR l) r)
unbalancedL B k v (Node R lk _ lv ll lr@(Node B _ _ _ _ _)) r
  = Unchanged (mNode B lk lv ll (balanceL B k v (turnR lr) r))
unbalancedL _ _ _ _ _ = error "unbalancedL"



-- | Remove the largest key from the map. Return the empty map if the map is empty.
deleteMax :: (Ord k) => IntervalMap k v -> IntervalMap k v
deleteMax Nil = Nil
deleteMax mp = case deleteMax' mp of
                 (Unchanged r, _ , _) -> makeBlack r
                 (Shrunk    r, _ , _) -> makeBlack r

deleteFindMin :: (Ord k) => IntervalMap k v -> ((Interval k,v), IntervalMap k v)
deleteFindMin mp = case deleteMin' mp of
                     (Unchanged r, k, v) -> ((k,v), makeBlack r)
                     (Shrunk    r, k, v) -> ((k,v), makeBlack r)

deleteFindMax :: (Ord k) => IntervalMap k v -> ((Interval k,v), IntervalMap k v)
deleteFindMax mp = case deleteMax' mp of
                     (Unchanged r, k, v) -> ((k,v), makeBlack r)
                     (Shrunk    r, k, v) -> ((k,v), makeBlack r)


-- folding

fold :: (a -> b -> b) -> b -> IntervalMap k a -> b
fold f z m = L.foldr f z (elems m)

foldrWithKey :: (Interval k -> v -> a -> a) -> a -> IntervalMap k v -> a
foldrWithKey f i m = go i m
 where
    go z Nil              = z
    go z (Node _ kx _ x l r) = go (f kx x (go z r)) l

foldlWithKey :: (a -> Interval k -> v -> a) -> a -> IntervalMap k v -> a
foldlWithKey f i m = go i m
 where
    go z Nil              = z
    go z (Node _ kx _ x l r) = go (f (go z l) kx x) r

delete :: (Ord k) => Interval k -> IntervalMap k v -> IntervalMap k v
delete key mp = case delete' key mp of
                  Unchanged r -> makeBlack r
                  Shrunk r    -> makeBlack r

delete' :: Ord k => Interval k -> IntervalMap k v -> DeleteResult k v
delete' _ Nil = Unchanged Nil
delete' x (Node c k _ v l r) =
  case compare x k of
    LT -> case delete' x l of
            (Unchanged l') -> Unchanged (mNode c k v l' r)
            (Shrunk l')    -> unbalancedR c k v l' r
    GT -> case delete' x r of
            (Unchanged r') -> Unchanged (mNode c k v l r')
            (Shrunk r')    -> unbalancedL c k v l r'
    EQ -> case r of
            Nil -> if c == B then blackify l else Unchanged l
            _ -> case deleteMin' r of
                   (Unchanged r', rk, rv) -> Unchanged (mNode c rk rv l r')
                   (Shrunk r', rk, rv) -> unbalancedL c rk rv l r'

blackify :: IntervalMap k v -> DeleteResult k v
blackify s@(Node R _ _ _ _ _) = Unchanged (makeBlack s)
blackify s                    = Shrunk s


adjust :: Ord k => (a -> a) -> Interval k -> IntervalMap k a -> IntervalMap k a
adjust f k m = adjustWithKey (\_ v -> f v) k m

adjustWithKey :: Ord k => (Interval k -> a -> a) -> Interval k -> IntervalMap k a -> IntervalMap k a
adjustWithKey _ _ Nil = Nil
adjustWithKey f x (Node c k m v l r) =
  case compare x k of
    LT -> Node c k m v (adjustWithKey f x l) r
    GT -> Node c k m v l (adjustWithKey f x r)
    EQ -> Node c k m (f k v) l r

update :: Ord k => (a -> Maybe a) -> Interval k -> IntervalMap k a -> IntervalMap k a
update f k m = updateWithKey (\_ v -> f v) k m

updateWithKey :: Ord k => (Interval k -> a -> Maybe a) -> Interval k -> IntervalMap k a -> IntervalMap k a
updateWithKey f k m = snd (updateLookupWithKey f k m)

updateLookupWithKey :: Ord k => (Interval k -> a -> Maybe a) -> Interval k -> IntervalMap k a -> (Maybe a, IntervalMap k a)
updateLookupWithKey f x m = case lookup x m of
                              Nothing -> (Nothing, m)
                              r@(Just v) -> case f x v of
                                              Nothing -> (r, delete x m)
                                              r'@(Just v') -> (r', adjust (const v') x m)

alter :: Ord k => (Maybe a -> Maybe a) -> Interval k -> IntervalMap k a -> IntervalMap k a
alter f x m = case lookup x m of
                Nothing -> case f Nothing of
                             Nothing -> m
                             Just v -> insert x v m
                y       -> case f y of
                             Nothing -> delete x m
                             Just v' -> adjust (const v') x m


union :: Ord k => IntervalMap k a -> IntervalMap k a -> IntervalMap k a
union m1 m2 = unionWith const m1 m2

unionWith :: Ord k => (a -> a -> a) -> IntervalMap k a -> IntervalMap k a -> IntervalMap k a
unionWith f m1 m2 = unionWithKey (\_ v1 v2 -> f v1 v2) m1 m2

unionWithKey :: Ord k => (Interval k -> a -> a -> a) -> IntervalMap k a -> IntervalMap k a -> IntervalMap k a
unionWithKey f m1 m2 = fromDistinctAscList (ascListUnion f (toAscList m1) (toAscList m2))

unions :: Ord k => [IntervalMap k a] -> IntervalMap k a
unions = foldl union empty

unionsWith :: Ord k => (a -> a -> a) -> [IntervalMap k a] -> IntervalMap k a
unionsWith f = foldl (unionWith f) empty

difference :: Ord k => IntervalMap k a -> IntervalMap k b -> IntervalMap k a
difference m1 m2 = differenceWithKey (\_ _ _ -> Nothing) m1 m2

differenceWith :: Ord k => (a -> b -> Maybe a) -> IntervalMap k a -> IntervalMap k b -> IntervalMap k a
differenceWith f m1 m2 = differenceWithKey (\_ v1 v2 -> f v1 v2) m1 m2

differenceWithKey :: Ord k => (Interval k -> a -> b -> Maybe a) -> IntervalMap k a -> IntervalMap k b -> IntervalMap k a
differenceWithKey f m1 m2 = fromDistinctAscList (ascListDifference f (toAscList m1) (toAscList m2))

intersection :: Ord k => IntervalMap k a -> IntervalMap k b -> IntervalMap k a
intersection m1 m2 = intersectionWithKey (\_ v _ -> v) m1 m2

intersectionWith :: Ord k => (a -> b -> c) -> IntervalMap k a -> IntervalMap k b -> IntervalMap k c
intersectionWith f m1 m2 = intersectionWithKey (\_ v1 v2 -> f v1 v2) m1 m2

intersectionWithKey :: Ord k => (Interval k -> a -> b -> c) -> IntervalMap k a -> IntervalMap k b -> IntervalMap k c
intersectionWithKey f m1 m2 = fromDistinctAscList (ascListIntersection f (toAscList m1) (toAscList m2))

ascListUnion :: Ord k => (k -> a -> a -> a) -> [(k,a)] -> [(k,a)] -> [(k,a)]
ascListUnion _ [] [] = []
ascListUnion _ [] ys = ys
ascListUnion _ xs [] = xs
ascListUnion f xs@(x@(xk,xv):xs') ys@(y@(yk,yv):ys') =
  case compare xk yk of
    LT -> x : ascListUnion f xs' ys
    GT -> y : ascListUnion f xs ys'
    EQ -> (xk, f xk xv yv) : ascListUnion f xs' ys'

ascListDifference :: Ord k => (k -> a -> b -> Maybe a) -> [(k,a)] -> [(k,b)] -> [(k,a)]
ascListDifference _ [] _  = []
ascListDifference _ xs [] = xs
ascListDifference f xs@(x@(xk,xv):xs') ys@((yk,yv):ys') =
  case compare xk yk of
    LT -> x : ascListDifference f xs' ys
    GT -> ascListDifference f xs ys'
    EQ -> case f xk xv yv of
            Nothing -> ascListDifference f xs' ys'
            Just v' -> (xk,v') : ascListDifference f xs' ys'

ascListIntersection :: Ord k => (k -> a -> b -> c) -> [(k,a)] -> [(k,b)] -> [(k,c)]
ascListIntersection _ [] _ = []
ascListIntersection _ _ [] = []
ascListIntersection f xs@((xk,xv):xs') ys@((yk,yv):ys') =
  case compare xk yk of
    LT -> ascListIntersection f xs' ys
    GT -> ascListIntersection f xs ys'
    EQ -> (xk, f xk xv yv) : ascListIntersection f xs' ys'


-- --- Conversion ---

-- | The list of all key\/value pairs contained in the map, in ascending order of keys.
toAscList :: IntervalMap k v -> [(Interval k,v)]
toAscList m = foldrWithKey (\k v r -> (k,v) : r) [] m

-- | The list of all key\/value pairs contained in the map, in no particular order.
toList :: IntervalMap k v -> [(Interval k,v)]
toList m = toAscList m

-- | The list of all key\/value pairs contained in the map, in descending order of keys.
toDescList :: IntervalMap k v -> [(Interval k, v)]
toDescList m = foldlWithKey (\r k v -> (k,v) : r) [] m

fromList :: Ord k => [(Interval k,v)] -> IntervalMap k v
fromList xs = L.foldl' (\m (k,v) -> insert k v m) empty xs

fromListWith :: Ord k => (a -> a -> a) -> [(Interval k,a)] -> IntervalMap k a 
fromListWith f xs = fromListWithKey (\_ x y -> f x y) xs

fromListWithKey :: Ord k => (Interval k -> a -> a -> a) -> [(Interval k,a)] -> IntervalMap k a 
fromListWithKey f xs = L.foldl' ins empty xs
  where
    ins t (k,x) = insertWithKey f k x t

fromAscList :: Ord k => [(Interval k,v)] -> IntervalMap k v
fromAscList xs = fromAscListWith (\_ b -> b) xs

fromAscListWith :: Ord k => (a -> a -> a) -> [(Interval k,a)] -> IntervalMap k a 
fromAscListWith f xs = fromAscListWithKey (\_ a b -> f a b) xs

fromAscListWithKey :: Ord k => (Interval k -> a -> a -> a) -> [(Interval k,a)] -> IntervalMap k a 
fromAscListWithKey f xs = fromDistinctAscList (combineEq f xs)

combineEq :: Eq k => (k -> a -> a -> a) -> [(k,a)] -> [(k,a)]
combineEq _ [] = []
combineEq _ xs@[_] = xs
combineEq f (x@(xk,xv) : xs@((yk,yv) : xs'))
  | xk == yk  = combineEq f ((xk, f xk xv yv) : xs')
  | otherwise = x : combineEq f xs


fromDistinctAscList :: (Ord k) => [(Interval k,v)] -> IntervalMap k v
-- exactly 2^n-1 items have height n. They can be all black
-- from 2^n - 2^n-2 items have height n+1. The lowest "row" should be red.
fromDistinctAscList lyst = case h (length lyst) lyst of
                             (result, []) -> result
                             _ -> error "fromDistinctAscList: list not fully consumed"
  where
    h n xs | n == 0      = (Nil, xs)
           | isPerfect n = buildB n xs
           | otherwise   = buildR n (log2 n) xs

    buildB n xs | n == 0     = error "fromDictinctAscList: buildB 0"
                | n == 1     = case xs of ((k,v):xs') -> (Node B k (uBound k) v Nil Nil, xs')
                | otherwise  =
                     case n `quot` 2 of { n' ->
                     case buildB n' xs of { (l, (k,v):xs') ->
                     case buildB n' xs' of { (r, xs'') ->
                     (mNode B k v l r, xs'') }}}

    buildR n d xs | n == 0    = (Nil, xs)
                  | n == 1    = case xs of ((k,v):xs') -> (Node (if d==0 then R else B) k (uBound k) v Nil Nil, xs')
                  | otherwise =
                      case n `quot` 2 of { n' ->
                      case buildR n' (d-1) xs of { (l, (k,v):xs') ->
                      case buildR (n - (n' + 1)) (d-1) xs' of { (r, xs'') ->
                      (mNode B k v l r, xs'') }}}

-- is n a perfect binary tree size (2^m-1)?
isPerfect :: Int -> Bool
isPerfect n = (n .&. (n + 1)) == 0

-- It's not worthwile to optimize this, as it's just called once in fromDistinctAscList.
log2 :: Int -> Int
log2 m = h (-1) m
  where
    h r n | n <= 0     = r
          | otherwise  = h (r + 1) (n `shiftR` 1)


-- | List of all values in the map, in no particular order.
elems :: IntervalMap k v -> [v]
elems m = [v | (_,v) <- toList m]

-- | List of all keys in the map, in no particular order.
keys :: IntervalMap k v -> [Interval k]
keys m = [k | (k,_) <- toList m]

-- | Set of the keys.
keysSet :: (Ord k) => IntervalMap k v -> Set.Set (Interval k)
keysSet m =  Set.fromList (keys m)

-- | Same as 'toList'.
assocs :: IntervalMap k v -> [(Interval k, v)]
assocs m = toList m

-- --- Mapping ---

-- | /O(n)/. Map a function over all values in the map.
map :: (a -> b) -> IntervalMap k a -> IntervalMap k b
map f = mapWithKey (\_ x -> f x)

-- | /O(n)/. Map a function over all values in the map.
mapWithKey :: (Interval k -> a -> b) -> IntervalMap k a -> IntervalMap k b
mapWithKey f = go
  where
    go Nil = Nil
    go (Node c k m v l r) = Node c k m (f k v) (go l) (go r)

-- | /O(n)/. The function 'mapAccum' threads an accumulating
-- argument through the map in ascending order of keys.
--
-- > let f a b = (a ++ b, b ++ "X")
-- > mapAccum f "Everything: " (fromList [(5,"a"), (3,"b")]) == ("Everything: ba", fromList [(3, "bX"), (5, "aX")])
mapAccum :: (a -> b -> (a,c)) -> a -> IntervalMap k b -> (a, IntervalMap k c)
mapAccum f a m
  = mapAccumWithKey (\a' _ x' -> f a' x') a m

-- | /O(n)/. The function 'mapAccumWithKey' threads an accumulating
-- argument through the map in ascending order of keys.
--
-- > let f a k b = (a ++ " " ++ (show k) ++ "-" ++ b, b ++ "X")
-- > mapAccumWithKey f "Everything:" (fromList [(5,"a"), (3,"b")]) == ("Everything: 3-b 5-a", fromList [(3, "bX"), (5, "aX")])
mapAccumWithKey :: (a -> Interval k -> b -> (a,c)) -> a -> IntervalMap k b -> (a, IntervalMap k c)
mapAccumWithKey f a t
  = mapAccumL f a t

-- | /O(n)/. The function 'mapAccumL' threads an accumulating
-- argument throught the map in ascending order of keys.
mapAccumL :: (a -> Interval k -> b -> (a,c)) -> a -> IntervalMap k b -> (a, IntervalMap k c)
mapAccumL f = go
  where
    go a Nil               = (a,Nil)
    go a (Node c kx m x l r) =
                 let (a1,l') = go a l
                     (a2,x') = f a1 kx x
                     (a3,r') = go a2 r
                 in (a3, Node c kx m x' l' r')

-- | /O(n)/. The function 'mapAccumR' threads an accumulating
-- argument through the map in descending order of keys.
mapAccumRWithKey :: (a -> Interval k -> b -> (a,c)) -> a -> IntervalMap k b -> (a, IntervalMap k c)
mapAccumRWithKey f = go
  where
    go a Nil = (a, Nil)
    go a (Node c kx m x l r) =
                 let (a1,r') = go a r
                     (a2,x') = f a1 kx x
                     (a3,l') = go a2 l
                 in (a3, Node c kx m x' l' r')


mapKeys :: Ord k2 => (Interval k1 -> Interval k2) -> IntervalMap k1 a -> IntervalMap k2 a
mapKeys f m = fromList [ (f k, v) | (k, v) <- toDescList m ]

mapKeysWith :: Ord k2 => (a -> a -> a) -> (Interval k1 -> Interval k2) -> IntervalMap k1 a -> IntervalMap k2 a
mapKeysWith c f m = fromListWith c [ (f k, v) | (k, v) <- toAscList m ]

mapKeysMonotonic :: Ord k2 => (Interval k1 -> Interval k2) -> IntervalMap k1 a -> IntervalMap k2 a
mapKeysMonotonic f m = mapKeys f m

filter :: Ord k => (a -> Bool) -> IntervalMap k a -> IntervalMap k a
filter p m = filterWithKey (\_ v -> p v) m

filterWithKey :: Ord k => (Interval k -> a -> Bool) -> IntervalMap k a -> IntervalMap k a
filterWithKey p m = mapMaybeWithKey (\k v -> if p k v then Just v else Nothing) m

partition :: Ord k => (a -> Bool) -> IntervalMap k a -> (IntervalMap k a, IntervalMap k a)
partition p m = partitionWithKey (\_ v -> p v) m

partitionWithKey :: Ord k => (Interval k -> a -> Bool) -> IntervalMap k a -> (IntervalMap k a, IntervalMap k a)
partitionWithKey p m = mapEitherWithKey p' m
  where
    p' k v | p k v     = Left v
           | otherwise = Right v

mapMaybe :: Ord k => (a -> Maybe b) -> IntervalMap k a -> IntervalMap k b
mapMaybe f m = mapMaybeWithKey (\_ v -> f v) m

mapMaybeWithKey :: Ord k => (Interval k -> a -> Maybe b) -> IntervalMap k a -> IntervalMap k b
mapMaybeWithKey f m = fromDistinctAscList (mapf [] m)
  where
    mapf z Nil = z
    mapf z (Node _ k _ v l r) = mapf (f' k v z r) l
    f' k v z r = case f k v of
                   Nothing -> mapf z r
                   Just v' -> (k,v') : mapf z r

mapEither :: Ord k => (a -> Either b c) -> IntervalMap k a -> (IntervalMap k b, IntervalMap k c)
mapEither f m = mapEitherWithKey (\_ v -> f v) m

mapEitherWithKey :: Ord k => (Interval k -> a -> Either b c) -> IntervalMap k a -> (IntervalMap k b, IntervalMap k c)
mapEitherWithKey f m = (fromDistinctAscList l, fromDistinctAscList r)
  where
    (l, r) = part [] [] (toDescList m)
    part ls rs [] = (ls, rs)
    part ls rs ((k,v):xs) = case f k v of
                              Left v'  -> part ((k,v'):ls) rs xs
                              Right v' -> part ls ((k,v'):rs) xs

split :: Ord k => Interval k -> IntervalMap k a -> (IntervalMap k a, IntervalMap k a)
split x m = (l, r)
  where (l, _, r) = splitLookup x m
                                     
splitLookup :: Ord k => Interval k -> IntervalMap k a -> (IntervalMap k a, Maybe a, IntervalMap k a)
splitLookup _ Nil = (Nil, Nothing, Nil)
splitLookup x (Node _ k _ v l r) = case compare x k of
                                     EQ -> (makeBlack l, Just v, makeBlack r)
                                     LT -> let (l', val, r') = splitLookup x l in (l', val, insert k v (union r r'))
                                     GT -> let (l', val, r') = splitLookup x r in (insert k v (union l l'), val, r')


-- debugging

-- | Check red-black-tree and search augmentation invariants.
valid :: Ord k => IntervalMap k v -> Bool
valid mp = ({-# SCC "scc_test" #-} test mp) && height mp <= maxHeight (size mp) && validColor mp
  where
    test Nil = True
    test n@(Node _ _ _ _ l r) = validOrder n && validMax n && test l && test r
    validMax (Node _ k m _ lo hi) =  m == maxUpper k lo hi
    validMax Nil = True

    validOrder (Node _ _ _ _ Nil Nil) = True
    validOrder (Node _ k1 _ _ Nil (Node _ k2 _ _ _ _)) = k1 < k2
    validOrder (Node _ k2 _ _ (Node _ k1 _ _ _ _) Nil) = k1 < k2
    validOrder (Node _ k2 _ _ (Node _ k1 _ _ _ _) (Node _ k3 _ _ _ _)) = k1 < k2 && k2 < k3
    validOrder Nil = True

    -- validColor parentColor blackCount tree
    validColor n = {-# SCC "scc_blackDepth" #-} blackDepth n >= 0

    -- return -1 if subtrees have diffrent black depths or two consecutive red nodes are encountered
    blackDepth :: IntervalMap k v -> Int
    blackDepth Nil  = 0
    blackDepth (Node c _ _ _ l r) = case blackDepth l of
                                      ld -> if ld < 0 then ld
                                            else
                                              case blackDepth r of
                                                rd -> if rd < 0 then rd
                                                      else if rd /= ld then -1
                                                      else if c == R && (isRed l || isRed r) then -1
                                                      else if c == B then rd + 1
                                                      else rd

-- --------------------------------
-- helper functions

makeBlack :: IntervalMap k v -> IntervalMap k v
makeBlack (Node R k m vs l r) = Node B k m vs l r
makeBlack t = t

mapSnd :: (b -> c) -> (a,b) -> (a,c)
mapSnd f (x,y) = (x, f y)


turnR :: IntervalMap k v -> IntervalMap k v
turnR Nil = error "turnR"
turnR (Node B k m v l r) = Node R k m v l r
turnR t = t
