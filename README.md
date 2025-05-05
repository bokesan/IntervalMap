# IntervalMap [![Hackage](https://img.shields.io/hackage/v/IntervalMap.svg)](https://hackage.haskell.org/package/IntervalMap) [![Continuous Integration](https://github.com/bokesan/IntervalMap/actions/workflows/ci.yml/badge.svg)]

Containers for intervals. Like `Data.Set` and `Data.Map` with
Intervals as keys and functions for efficiently getting the subset
of all intervals containing a point, intersecting an interval, and more.

Home page and documentation: [http://www.chr-breitkopf.de/comp/IntervalMap/index.html](http://www.chr-breitkopf.de/comp/IntervalMap/index.html)

## Getting started

Enable necessary language extensions:
```haskell
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
```
In most cases, you should use the value-strict version:
```haskell
import qualified Data.IntervalMap.Generic.Strict as IM
```
Make tuples an instance of Interval:
```haskell
instance Ord e => IM.Interval (e,e) e where
    lowerBound (a,_) = a
    upperBound (_,b) = b
    rightClosed _ = False
```
By using `rightClosed _ = False` we have defined tuples to be half-open
intervals - they include the starting value, but not the end value.

Let's create a map from `(Int,Int)` intervals to strings:
```haskell
type MyMap = IM.IntervalMap (Int,Int) String

sample :: MyMap
sample = IM.fromList [((1,6), "Foo"), ((2,4), "Bar"), ((4,7), "Baz")]
```
Lookup intervals containing a given point ("stabbing query"):
```
> IM.toAscList (sample `IM.containing` 3)
[((1,6),"Foo"),((2,4),"Bar")]
> IM.toAscList (sample `IM.containing` 4)
[((1,6),"Foo"),((4,7),"Baz")]
```
