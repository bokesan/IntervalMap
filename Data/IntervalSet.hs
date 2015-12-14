-- |
-- Module      :  Data.IntervalSet
-- Copyright   :  (c) Christoph Breitkopf 2015
-- License     :  BSD-style
-- Maintainer  :  chbreitkopf@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (MPTC with FD)
--
-- An implementation of sets of intervals. The intervals may
-- overlap, and the implementation contains efficient search functions
-- for all intervals containing a point or overlapping a given interval.
-- Closed, open, and half-open intervals can be contained in the same set.
--
-- It is an error to insert an empty interval into a set. This precondition is not
-- checked by the various construction functions.
--
-- Since many function names (but not the type name) clash with
-- /Prelude/ names, this module is usually imported @qualified@, e.g.
--
-- >  import Data.IntervalSet.Strict (IntervalSet)
-- >  import qualified Data.IntervalSet.Strict as IS
--
-- It offers most of the same functions as 'Data.Set', but the member type must be an
-- instance of 'Interval'. The 'findMin' and 'findMax' functions deviate from their
-- set counterparts in being total and returning a 'Maybe' value.
-- Some functions differ in asymptotic performance (for example 'size') or have not
-- been tuned for efficiency as much as their equivalents in 'Data.Set'.
--
-- In addition, there are functions specific to sets of intervals, for example to search
-- for all intervals containing a given point or contained in a given interval.
--
-- The implementation is a red-black tree augmented with the maximum upper bound
-- of all keys.
--
-- Parts of this implementation are based on code from the 'Data.Map' implementation,
-- (c) Daan Leijen 2002, (c) Andriy Palamarchuk 2008.
-- The red-black tree deletion is based on code from llrbtree by Kazu Yamamoto.
-- Of course, any errors are mine.
--
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.IntervalSet (
            -- * re-export
            Interval(..)
            -- * Set type
            , IntervalSet(..)      -- instance Eq,Show,Read

            -- * Operators
            , (\\)

            -- * Query
            , null
            , size
            , member
            , notMember

            -- ** Interval query
            , containing
            , intersecting
            , within
            
            -- * Construction
            , empty
            , singleton

            -- ** Insertion
            , insert
            
            -- ** Delete\/Update
            , delete

            -- * Combine
            , union
            , unions
            , difference
            , intersection

            -- * Traversal
            -- ** Map
            , map
            , mapMonotonic

            -- ** Fold
            , foldr, foldl
            , foldl', foldr'

            -- * Conversion
            , elems

            -- ** Lists
            , toList
            , fromList

            -- ** Ordered lists
            , toAscList
            , toDescList
            , fromAscList
            , fromDistinctAscList

            -- * Filter
            , filter
            , partition

            , split
            , splitMember
            , splitAt
            , splitAround

            -- * Subset
            , isSubsetOf, isProperSubsetOf

            -- * Min\/Max
            , findMin
            , findMax
            , findLast
            , deleteMin
            , deleteMax
            , deleteFindMin
            , deleteFindMax
            , minView
            , maxView

            -- * Debugging
            , valid

            ) where

import Prelude hiding (null, lookup, map, filter, foldr, foldl, splitAt)
import Data.Bits (shiftR, (.&.))
import Data.Monoid (Monoid(..))
import qualified Data.Foldable as Foldable
import qualified Data.List as L
import Control.DeepSeq
import qualified Data.Foldable as Foldable

import Data.IntervalMap.Generic.Interval

{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}
infixl 9 \\ --

-- | Same as 'difference'.
(\\) :: (Interval k e, Ord k) => IntervalSet k -> IntervalSet k -> IntervalSet k
m1 \\ m2 = difference m1 m2


data Color = R | B deriving (Eq)

-- | A set of intervals of type @k@.
data IntervalSet k = Nil
                   | Node !Color
                          !k -- key
                          !k -- interval with maximum upper in tree
                          !(IntervalSet k) -- left subtree
                          !(IntervalSet k) -- right subtree

instance (Eq k) => Eq (IntervalSet k) where
  a == b = toAscList a == toAscList b

instance (Ord k) => Ord (IntervalSet k) where
  compare a b = compare (toAscList a) (toAscList b)

instance (Interval i k, Ord i) => Monoid (IntervalSet i) where
    mempty  = empty
    mappend = union
    mconcat = unions
              
instance Foldable.Foldable IntervalSet where
    fold t = go t
      where go Nil = mempty
            go (Node _ k _ l r) = go l `mappend` (k `mappend` go r)
    foldr = foldr
    foldl = foldl
    foldMap f t = go t
      where go Nil = mempty
            go (Node _ k _ l r) = go l `mappend` (f k `mappend` go r)

instance (NFData k) => NFData (IntervalSet k) where
    rnf Nil = ()
    rnf (Node _ kx _ l r) = kx `deepseq` l `deepseq` r `deepseq` ()

instance (Ord k, Read k, Interval i k, Ord i, Read i) => Read (IntervalSet i) where
  readsPrec p = readParen (p > 10) $ \ r -> do
    ("fromList",s) <- lex r
    (xs,t) <- reads s
    return (fromList xs,t)

instance (Show k) => Show (IntervalSet k) where
  showsPrec d m  = showParen (d > 10) $
    showString "fromList " . shows (toList m)


isRed :: IntervalSet k -> Bool
isRed (Node R _ _ _ _) = True
isRed _ = False

turnBlack :: IntervalSet k -> IntervalSet k
turnBlack (Node R k m l r) = Node B k m l r
turnBlack t = t

turnRed :: IntervalSet k -> IntervalSet k
turnRed Nil = error "turnRed: Leaf"
turnRed (Node B k m l r) = Node R k m l r
turnRed t = t

-- construct node, recomputing the upper key bound.
mNode :: (Interval k e) => Color -> k -> IntervalSet k -> IntervalSet k -> IntervalSet k
mNode c k l r = Node c k (maxUpper k l r) l r

maxUpper :: (Interval i k) => i -> IntervalSet i -> IntervalSet i -> i
maxUpper k Nil              Nil              = k `seq` k
maxUpper k Nil              (Node _ _ m _ _) = maxByUpper k m
maxUpper k (Node _ _ m _ _) Nil              = maxByUpper k m
maxUpper k (Node _ _ l _ _) (Node _ _ r _ _) = maxByUpper k (maxByUpper l r)

-- interval with the greatest upper bound. The lower bound is ignored!
maxByUpper :: (Interval i e) => i -> i -> i
maxByUpper a b | rightClosed a = if upperBound a >= upperBound b then a else b
               | otherwise     = if upperBound a >  upperBound b then a else b

-- ---------------------------------------------------------

-- | /O(1)/. The empty set.
empty :: IntervalSet k
empty =  Nil

-- | /O(1)/. A set with one entry.
singleton :: k -> IntervalSet k
singleton k = Node B k k Nil Nil


-- | /O(1)/. Is the set empty?
null :: IntervalSet k -> Bool
null Nil = True
null _   = False

-- | /O(n)/. Number of keys in the set.
--
-- Caution: unlike 'Data.Set.size', this takes linear time!
size :: IntervalSet k -> Int
size t = h 0 t
  where
    h n s = n `seq` case s of
                      Nil -> n
                      Node _ _ _ l r -> h (h n l + 1) r

-- | /O(log n)/. Does the set contain the given value? See also 'notMember'.
member :: (Ord k) => k -> IntervalSet k -> Bool
member k Nil = k `seq` False
member k (Node _ key _ l r) = case compare k key of
                                LT -> member k l
                                GT -> member k r
                                EQ -> True

-- | /O(log n)/. Does the set not contain the given value? See also 'member'.
notMember :: (Ord k) => k -> IntervalSet k -> Bool
notMember key tree = not (member key tree)

-- | Return the set of all intervals containing the given point.
--
-- /O(n)/, since potentially all intervals could contain the point.
-- /O(log n)/ average case. This is also the worst case for sets containing no overlapping intervals.
containing :: (Interval k e) => IntervalSet k -> e -> IntervalSet k
t `containing` p = p `seq` fromDistinctAscList (go [] t)
  where
    go xs Nil = xs
    go xs (Node _ k m l r)
       | p `above` m  =  xs         -- above all intervals in the tree: no result
       | p `below` k  =  go xs l    -- to the left of the lower bound: can't be in right subtree
       | p `inside` k =  go (k : go xs r) l
       | otherwise    =  go (go xs r) l

-- | Return the set of all intervals overlapping (intersecting) the given interval.
--
-- /O(n)/, since potentially all values could intersect the interval.
-- /O(log n)/ average case, if few values intersect the interval.
intersecting :: (Interval k e) => IntervalSet k -> k -> IntervalSet k
t `intersecting` i = i `seq` fromDistinctAscList (go [] t)
  where
    go xs Nil = xs
    go xs (Node _ k m l r)
       | i `after` m     =  xs
       | i `before` k    =  go xs l
       | i `overlaps` k  =  go (k : go xs r) l
       | otherwise       =  go (go xs r) l

-- | Return the set of all intervals which are completely inside the given interval.
--
-- /O(n)/, since potentially all values could be inside the interval.
-- /O(log n)/ average case, if few keys are inside the interval.
within :: (Interval k e) => IntervalSet k -> k -> IntervalSet k
t `within` i = i `seq` fromDistinctAscList (go [] t)
  where
    go xs Nil = xs
    go xs (Node _ k m l r)
       | i `after` m     =  xs
       | i `before` k    =  go xs l
       | i `subsumes` k  =  go (k : go xs r) l
       | otherwise       =  go (go xs r) l


-- | /O(log n)/. Insert a new value. If the set already contains an element equal to the value,
-- it is replaced by the new value.
insert :: (Interval k e, Ord k) => k -> IntervalSet k -> IntervalSet k
insert v s = v `seq` turnBlack (ins s)
  where
    singletonR k = Node R k k Nil Nil
    ins Nil = singletonR v
    ins (Node color k m l r) =
      case compare v k of
        LT -> balanceL color k (ins l) r
        GT -> balanceR color k l (ins r)
        EQ -> Node color v m l r

balanceL :: (Interval k e) => Color -> k -> IntervalSet k -> IntervalSet k -> IntervalSet k
balanceL B zk (Node R yk _ (Node R xk _ a b) c) d =
    mNode R yk (mNode B xk a b) (mNode B zk c d)
balanceL B zk (Node R xk _ a (Node R yk _ b c)) d =
    mNode R yk (mNode B xk a b) (mNode B zk c d)
balanceL c xk l r = mNode c xk l r

balanceR :: (Interval k e) => Color -> k -> IntervalSet k -> IntervalSet k -> IntervalSet k
balanceR B xk a (Node R yk _ b (Node R zk _ c d)) =
    mNode R yk (mNode B xk a b) (mNode B zk c d)
balanceR B xk a (Node R zk _ (Node R yk _ b c) d) =
    mNode R yk (mNode B xk a b) (mNode B zk c d)
balanceR c xk l r = mNode c xk l r


-- min/max

-- | /O(log n)/. Returns the least interval in the set.
findMin :: IntervalSet k -> Maybe k
findMin (Node _ k _ Nil _) = Just k
findMin (Node _ _ _ l _) = findMin l
findMin Nil = Nothing

-- | /O(log n)/. Returns the largest interval in the set.
findMax :: IntervalSet k -> Maybe k
findMax (Node _ k _ _ Nil) = Just k
findMax (Node _ _ _ _ r) = findMax r
findMax Nil = Nothing

-- | Returns the interval with the largest endpoint.
-- If there is more than one interval with that endpoint,
-- return the rightmost.
--
-- /O(n)/, since all intervals could have the same endpoint.
-- /O(log n)/ average case.
findLast :: (Interval k e) => IntervalSet k -> Maybe k
findLast Nil = Nothing
findLast t@(Node _ _ mx _ _) = go t
  where
    go (Node _ k m l r) | sameU m mx = if sameU k m then go r `orElse` Just k
                                                    else go r `orElse` go l
                        | otherwise  = Nothing
    go Nil = Nothing
    sameU a b = upperBound a == upperBound b && rightClosed a == rightClosed b
    Nothing `orElse` x = x
    x       `orElse` _ = x


-- Type to indicate whether the number of black nodes changed or stayed the same.
data DeleteResult k = U !(IntervalSet k)   -- Unchanged
                    | S !(IntervalSet k)   -- Shrunk

unwrap :: DeleteResult k -> IntervalSet k
unwrap (U m) = m
unwrap (S m) = m

-- DeleteResult with value
data DeleteResult' k a = U' !(IntervalSet k) a
                       | S' !(IntervalSet k) a

unwrap' :: DeleteResult' k a -> IntervalSet k
unwrap' (U' m _) = m
unwrap' (S' m _) = m

-- annotate DeleteResult with value
annotate :: DeleteResult k -> a -> DeleteResult' k a
annotate (U m) x = U' m x
annotate (S m) x = S' m x


-- | /O(log n)/. Remove the smallest element from the set. Return the empty set if the set is empty.
deleteMin :: (Interval k e, Ord k) => IntervalSet k -> IntervalSet k
deleteMin Nil = Nil
deleteMin m   = turnBlack (unwrap' (deleteMin' m))

deleteMin' :: (Interval k e, Ord k) => IntervalSet k -> DeleteResult' k k
deleteMin' Nil = error "deleteMin': Nil"
deleteMin' (Node B k _ Nil Nil) = S' Nil k
deleteMin' (Node B k _ Nil r@(Node R _ _ _ _)) = U' (turnBlack r) k
deleteMin' (Node R k _ Nil r) = U' r k
deleteMin' (Node c k _ l r) =
  case deleteMin' l of
    (U' l' kv) -> U' (mNode c k l' r) kv
    (S' l' kv) -> annotate (unbalancedR c k l' r) kv

deleteMax' :: (Interval k e, Ord k) => IntervalSet k -> DeleteResult' k k
deleteMax' Nil = error "deleteMax': Nil"
deleteMax' (Node B k _ Nil Nil) = S' Nil k
deleteMax' (Node B k _ l@(Node R _ _ _ _) Nil) = U' (turnBlack l) k
deleteMax' (Node R k _ l Nil) = U' l k
deleteMax' (Node c k _ l r) =
  case deleteMax' r of
    (U' r' kv) -> U' (mNode c k l r') kv
    (S' r' kv) -> annotate (unbalancedL c k l r') kv

-- The left tree lacks one Black node
unbalancedR :: (Interval k e, Ord k) => Color -> k -> IntervalSet k -> IntervalSet k -> DeleteResult k
-- Decreasing one Black node in the right
unbalancedR B k l r@(Node B _ _ _ _) = S (balanceR B k l (turnRed r))
unbalancedR R k l r@(Node B _ _ _ _) = U (balanceR B k l (turnRed r))
-- Taking one Red node from the right and adding it to the right as Black
unbalancedR B k l (Node R rk _ rl@(Node B _ _ _ _) rr)
  = U (mNode B rk (balanceR B k l (turnRed rl)) rr)
unbalancedR _ _ _ _ = error "unbalancedR"

unbalancedL :: (Interval k e, Ord k) => Color -> k -> IntervalSet k -> IntervalSet k -> DeleteResult k
unbalancedL R k l@(Node B _ _ _ _) r = U (balanceL B k (turnRed l) r)
unbalancedL B k l@(Node B _ _ _ _) r = S (balanceL B k (turnRed l) r)
unbalancedL B k (Node R lk _ ll lr@(Node B _ _ _ _)) r
  = U (mNode B lk ll (balanceL B k (turnRed lr) r))
unbalancedL _ _ _ _ = error "unbalancedL"


-- | /O(log n)/. Remove the largest element from the set. Return the empty set if the set is empty.
deleteMax :: (Interval k e, Ord k) => IntervalSet k -> IntervalSet k
deleteMax Nil = Nil
deleteMax m   = turnBlack (unwrap' (deleteMax' m))

-- | /O(log n)/. Delete and return the smallest element.
deleteFindMin :: (Interval k e, Ord k) => IntervalSet k -> (k, IntervalSet k)
deleteFindMin mp = case deleteMin' mp of
                     (U' r v) -> (v, turnBlack r)
                     (S' r v) -> (v, turnBlack r)

-- | /O(log n)/. Delete and return the largest element.
deleteFindMax :: (Interval k e, Ord k) => IntervalSet k -> (k, IntervalSet k)
deleteFindMax mp = case deleteMax' mp of
                     (U' r v) -> (v, turnBlack r)
                     (S' r v) -> (v, turnBlack r)

-- | /O(log n)/. Retrieves the minimal element of the set, and
-- the set stripped of that element, or 'Nothing' if passed an empty set.
minView :: (Interval k e, Ord k) => IntervalSet k -> Maybe (k, IntervalSet k)
minView Nil = Nothing
minView x   = Just (deleteFindMin x)

-- | /O(log n)/. Retrieves the maximal element of the set, and
-- the set stripped of that element, or 'Nothing' if passed an empty set.
maxView :: (Interval k e, Ord k) => IntervalSet k -> Maybe (k, IntervalSet k)
maxView Nil = Nothing
maxView x   = Just (deleteFindMax x)


-- folding

-- | /O(n)/. Fold the values in the set using the given right-associative
-- binary operator, such that @'foldr' f z == 'Prelude.foldr' f z . 'elems'@.
foldr :: (k -> b -> b) -> b -> IntervalSet k -> b
foldr _ z Nil = z
foldr f z (Node _ k _ l r) = foldr f (f k (foldr f z r)) l

-- | /O(n)/. A strict version of 'foldr'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldr' :: (k -> b -> b) -> b -> IntervalSet k -> b
foldr' f z s = z `seq` case s of
                         Nil -> z
                         Node _ k _ l r -> foldr' f (f k (foldr' f z r)) l

-- | /O(n)/. Fold the values in the set using the given left-associative
-- binary operator, such that @'foldl' f z == 'Prelude.foldl' f z . 'elems'@.
foldl :: (b -> k -> b) -> b -> IntervalSet k -> b
foldl _ z Nil = z
foldl f z (Node _ k _ l r) = foldl f (f (foldl f z l) k) r

-- | /O(n)/. A strict version of 'foldl'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldl' :: (b -> k -> b) -> b -> IntervalSet k -> b
foldl' f z s = z `seq` case s of
                         Nil -> z
                         Node _ k _ l r -> foldl' f (f (foldl' f z l) k) r

-- delete

-- | /O(log n)/. Delete an element from the set. If the set does not contain the value,
-- it is returned unchanged.
delete :: (Interval k e, Ord k) => k -> IntervalSet k -> IntervalSet k
delete key mp = turnBlack (unwrap (delete' key mp))

delete' :: (Interval k e, Ord k) => k -> IntervalSet k -> DeleteResult k
delete' x Nil = x `seq` U Nil
delete' x (Node c k _ l r) =
  case compare x k of
    LT -> case delete' x l of
            (U l') -> U (mNode c k l' r)
            (S l')    -> unbalancedR c k l' r
    GT -> case delete' x r of
            (U r') -> U (mNode c k l r')
            (S r')    -> unbalancedL c k l r'
    EQ -> case r of
            Nil -> if c == B then blackify l else U l
            _ -> case deleteMin' r of
                   (U' r' rk) -> U (mNode c rk l r')
                   (S' r' rk) -> unbalancedL c rk l r'

blackify :: IntervalSet k -> DeleteResult k
blackify (Node R k m l r) = U (Node B k m l r)
blackify s                = S s


-- | /O(n+m)/. The expression (@'union' t1 t2@) takes the left-biased union of @t1@ and @t2@. 
-- It prefers @t1@ when duplicate elements are encountered.
union :: (Interval k e, Ord k) => IntervalSet k -> IntervalSet k -> IntervalSet k
union m1 m2 = fromDistinctAscList (ascListUnion (toAscList m1) (toAscList m2))

-- | The union of a list of sets:
--   (@'unions' == 'Prelude.foldl' 'union' 'empty'@).
unions :: (Interval k e, Ord k) => [IntervalSet k] -> IntervalSet k
unions []  = empty
unions [s] = s
unions iss = fromDistinctAscList (head (go (L.map toAscList iss)))
  where
    go []       = []
    go xs@[_]   = xs
    go (x:y:xs) = go (ascListUnion x y : go xs)

-- | /O(n+m)/. Difference of two sets.
-- Return elements of the first set not existing in the second set.
difference :: (Interval k e, Ord k) => IntervalSet k -> IntervalSet k -> IntervalSet k
difference m1 m2 = fromDistinctAscList (ascListDifference (toAscList m1) (toAscList m2))

-- | /O(n+m)/. Intersection of two sets.
-- Return elements in the first set also existing in the second set.
intersection :: (Interval k e, Ord k) => IntervalSet k -> IntervalSet k -> IntervalSet k
intersection m1 m2 = fromDistinctAscList (ascListIntersection (toAscList m1) (toAscList m2))

ascListUnion :: Ord k => [k] -> [k] -> [k]
ascListUnion [] [] = []
ascListUnion [] ys = ys
ascListUnion xs [] = xs
ascListUnion xs@(x:xs') ys@(y:ys') =
  case compare x y of
    LT -> x : ascListUnion xs' ys
    GT -> y : ascListUnion xs ys'
    EQ -> x : ascListUnion xs' ys'

ascListDifference :: Ord k => [k] -> [k] -> [k]
ascListDifference [] _  = []
ascListDifference xs [] = xs
ascListDifference xs@(xk:xs') ys@(yk:ys') =
  case compare xk yk of
    LT -> xk : ascListDifference xs' ys
    GT -> ascListDifference xs ys'
    EQ -> ascListDifference xs' ys'

ascListIntersection :: Ord k => [k] -> [k] -> [k]
ascListIntersection [] _ = []
ascListIntersection _ [] = []
ascListIntersection xs@(xk:xs') ys@(yk:ys') =
  case compare xk yk of
    LT -> ascListIntersection xs' ys
    GT -> ascListIntersection xs ys'
    EQ -> xk : ascListIntersection xs' ys'


-- --- Conversion ---

-- | /O(n)/. The list of all values contained in the set, in ascending order.
toAscList :: IntervalSet k -> [k]
toAscList m = foldr (\k r -> k : r) [] m

toAscList' :: IntervalSet k -> [k] -> [k]
toAscList' m xs = foldr (\k r -> k : r) xs m



-- | /O(n)/. The list of all values in the set, in no particular order.
toList :: IntervalSet k -> [k]
toList s = go s []
  where
    go Nil              xs = xs
    go (Node _ k _ l r) xs = k : go l (go r xs)

-- | /O(n)/. The list of all values in the set, in descending order.
toDescList :: IntervalSet k -> [k]
toDescList m = foldl (\r k -> k : r) [] m

-- | /O(n log n)/. Build a set from a list of elements. See also 'fromAscList'.
-- If the list contains duplicate values, the last value is retained.
fromList :: (Interval k e, Ord k) => [k] -> IntervalSet k
fromList xs = L.foldl' (\m k -> insert k m) empty xs

-- | /O(n)/. Build a set from an ascending list in linear time.
-- /The precondition (input list is ascending) is not checked./
fromAscList :: (Interval k e, Eq k) => [k] -> IntervalSet k
fromAscList xs = fromDistinctAscList (uniq xs)

uniq :: Eq k => [k] -> [k]
uniq [] = []
uniq (x:xs) = go x xs
  where
    go v [] = [v]
    go v (y:ys) | v == y    = go v ys
                | otherwise = v : go y ys
                              
-- Strict tuple
data T2 a b = T2 !a !b


-- | /O(n)/. Build a set from an ascending list of distinct elements in linear time.
-- /The precondition is not checked./
fromDistinctAscList :: (Interval k e) => [k] -> IntervalSet k
-- exactly 2^n-1 items have height n. They can be all black
-- from 2^n - 2^n-2 items have height n+1. The lowest "row" should be red.
fromDistinctAscList lyst = case h (length lyst) lyst of
                             (T2 result []) -> result
                             _ -> error "fromDistinctAscList: list not fully consumed"
  where
    h n xs | n == 0      = T2 Nil xs
           | isPerfect n = buildB n xs
           | otherwise   = buildR n (log2 n) xs

    buildB n xs | xs `seq` n <= 0 = error "fromDictinctAscList: buildB 0"
                | n == 1     = case xs of (k:xs') -> T2 (Node B k k Nil Nil) xs'
                                          _ -> error "fromDictinctAscList: buildB 1"
                | otherwise  =
                     case n `quot` 2 of { n' ->
                     case buildB n' xs of { (T2 _ []) -> error "fromDictinctAscList: buildB n";
                                            (T2 l (k:xs')) ->
                     case buildB n' xs' of { (T2 r xs'') ->
                     T2 (mNode B k l r) xs'' }}}

    buildR n d xs | d `seq` xs `seq` n == 0 = T2 Nil xs
                  | n == 1    = case xs of (k:xs') -> T2 (Node (if d==0 then R else B) k k Nil Nil) xs'
                                           _ -> error "fromDistinctAscList: buildR 1"
                  | otherwise =
                      case n `quot` 2 of { n' ->
                      case buildR n' (d-1) xs of { (T2 _ []) -> error "fromDistinctAscList: buildR n";
                                                   (T2 l (k:xs')) ->
                      case buildR (n - (n' + 1)) (d-1) xs' of { (T2 r xs'') ->
                      T2 (mNode B k l r) xs'' }}}


-- is n a perfect binary tree size (2^m-1)?
isPerfect :: Int -> Bool
isPerfect n = (n .&. (n + 1)) == 0

log2 :: Int -> Int
log2 m = h (-1) m
  where
    h r n | r `seq` n <= 0 = r
          | otherwise      = h (r + 1) (n `shiftR` 1)


-- | /O(n)/. List of all values in the set, in ascending order.
elems :: IntervalSet k -> [k]
elems s = toAscList s

-- --- Mapping ---

-- | /O(n log n)/. Map a function over all values in the set.
--
-- The size of the result may be smaller if @f@ maps two or more distinct
-- elements to the same value.
map :: (Interval a e1, Interval b e2, Ord b) => (a -> b) -> IntervalSet a -> IntervalSet b
map f s = fromList [f x | x <- toList s]

-- | /O(n)/. @'mapMonotonic' f s == 'map' f s@, but works only when @f@
-- is strictly monotonic.
-- That is, for any values @x@ and @y@, if @x@ < @y@ then @f x@ < @f y@.
-- /The precondition is not checked./
mapMonotonic :: (Interval k2 e, Ord k2) => (k1 -> k2) -> IntervalSet k1 -> IntervalSet k2
mapMonotonic _ Nil = Nil
mapMonotonic f (Node c k _ l r) =
    mNode c (f k) (mapMonotonic f l) (mapMonotonic f r)

-- | /O(n)/. Filter values satisfying a predicate.
filter :: (Interval k e) => (k -> Bool) -> IntervalSet k -> IntervalSet k
filter p s = fromDistinctAscList (L.filter p (toAscList s))

-- | /O(n)/. Partition the set according to a predicate. The first
-- set contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
partition :: (Interval k e) => (k -> Bool) -> IntervalSet k -> (IntervalSet k, IntervalSet k)
partition p s = let (xs,ys) = L.partition p (toAscList s)
                in (fromDistinctAscList xs, fromDistinctAscList ys)

-- | /O(n)/. The expression (@'split' k set@) is a pair @(set1,set2)@ where
-- the elements in @set1@ are smaller than @k@ and the elements in @set2@ larger than @k@.
-- Any key equal to @k@ is found in neither @set1@ nor @set2@.
split :: (Interval i k, Ord i) => i -> IntervalSet i -> (IntervalSet i, IntervalSet i)
split x m = (l, r)
  where (l, _, r) = splitMember x m
     
-- | /O(n)/. The expression (@'splitMember' k set@) splits a set just
-- like 'split' but also returns @'member' k set@.
splitMember :: (Interval i k, Ord i) => i -> IntervalSet i -> (IntervalSet i, Bool, IntervalSet i)
splitMember x s = case span (< x) (toAscList s) of
                    ([], [])                    -> (empty, False, empty)
                    ([], (y:_))     | y == x    -> (empty, True, deleteMin s)
                                    | otherwise -> (empty, False, s)
                    (_, [])                     -> (s, False, empty)
                    (lt, ge@(y:gt)) | y == x    -> (fromDistinctAscList lt, True, fromDistinctAscList gt)
                                    | otherwise -> (fromDistinctAscList lt, False, fromDistinctAscList ge)

-- Helper for building sets
data Union k = UEmpty | Union !(Union k) !(Union k)
             | UCons !k !(Union k)
             | UAppend !(IntervalSet k) !(Union k)

mkUnion :: Union a -> Union a -> Union a
mkUnion UEmpty u = u
mkUnion u UEmpty = u
mkUnion u1 u2 = Union u1 u2

fromUnion :: Interval k e => Union k -> IntervalSet k
fromUnion UEmpty               = empty
fromUnion (UCons key UEmpty)   = singleton key
fromUnion (UAppend set UEmpty) = turnBlack set
fromUnion x                    = fromDistinctAscList (unfold x [])
  where
    unfold UEmpty        r = r
    unfold (Union a b)   r = unfold a (unfold b r)
    unfold (UCons k u)   r = k : unfold u r
    unfold (UAppend s u) r = toAscList' s (unfold u r)

-- | /O(n)/. Split around a point.
splitAt :: (Interval i k) => k -> IntervalSet i -> (IntervalSet i, IntervalSet i, IntervalSet i)
splitAt p set = (fromUnion (lower set), set `containing` p, fromUnion (higher set))
  where
    lower Nil = UEmpty
    lower s@(Node _ k m l r)
      | p `above`  m  =  UAppend s UEmpty
      | p `below`  k  =  lower l
      | p `inside` k  =  mkUnion (lower l) (lower r)
      | otherwise     =  mkUnion (lower l) (UCons k (lower r))
    higher Nil = UEmpty
    higher (Node _ k m l r)
      | p `above`  m  =  UEmpty
      | p `below`  k  =  mkUnion (higher l) (UCons k (UAppend r UEmpty))
      | otherwise     =  higher r

-- | /O(n)/. Split around an interval.
splitAround :: (Interval i k) => i -> IntervalSet i -> (IntervalSet i, IntervalSet i, IntervalSet i)
splitAround i s = (fromDistinctAscList [x | x <- toAscList s, x `before` i],
                   s `intersecting` i,
                   fromDistinctAscList [x | x <- toAscList s, x `after` i])


-- subsets

-- | /O(n+m)/.
isSubsetOf :: (Ord k) => IntervalSet k -> IntervalSet k -> Bool
isSubsetOf m1 m2 = go (toAscList m1) (toAscList m2)
  where
    go []    _  =  True
    go (_:_) [] =  False
    go s1@(k1:r1) (k2:r2) =
       case compare k1 k2 of
         GT -> go s1 r2
         EQ -> go r1 r2
         LT -> False

-- | /O(n+m)/. Is this a proper subset? (ie. a subset but not equal). 
isProperSubsetOf :: (Ord k) => IntervalSet k -> IntervalSet k -> Bool
isProperSubsetOf m1 m2 = size m1 < size m2 && isSubsetOf m1 m2

-- debugging


-- | The height of the tree. For testing/debugging only.
height :: IntervalSet k -> Int
height Nil = 0
height (Node _ _ _ l r) = 1 + max (height l) (height r)

-- | The maximum height of a red-black tree with the given number of nodes.
-- For testing/debugging only.
maxHeight :: Int -> Int
maxHeight nodes = 2 * log2 (nodes + 1)


-- | Check red-black-tree and interval search augmentation invariants.
-- For testing/debugging only.
valid :: (Interval i k, Ord i) => IntervalSet i -> Bool
valid mp = test mp && height mp <= maxHeight (size mp) && validColor mp
  where
    test Nil = True
    test n@(Node _ _ _ l r) = validOrder n && validMax n && test l && test r
    validMax (Node _ k m lo hi) =  m == maxUpper k lo hi
    validMax Nil = True

    validOrder (Node _ _ _ Nil Nil) = True
    validOrder (Node _ k1 _ Nil (Node _ k2 _ _ _)) = k1 < k2
    validOrder (Node _ k2 _ (Node _ k1 _ _ _) Nil) = k1 < k2
    validOrder (Node _ k2 _ (Node _ k1 _ _ _) (Node _ k3 _ _ _)) = k1 < k2 && k2 < k3
    validOrder Nil = True

    -- validColor parentColor blackCount tree
    validColor n = blackDepth n >= 0

    -- return -1 if subtrees have diffrent black depths or two consecutive red nodes are encountered
    blackDepth :: IntervalSet k -> Int
    blackDepth Nil  = 0
    blackDepth (Node c _ _ l r) = case blackDepth l of
                                      ld -> if ld < 0 then ld
                                            else
                                              case blackDepth r of
                                                rd -> if rd < 0 then rd
                                                      else if rd /= ld then -1
                                                      else if c == R && (isRed l || isRed r) then -1
                                                      else if c == B then rd + 1
                                                      else rd

