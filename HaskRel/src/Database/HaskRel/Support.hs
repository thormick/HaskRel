
{-|
Module      : Support
Description : Predicates and aggregates useful to relational querying
Copyright   : © Thor Michael Støre, 2015
License     : GPL v2 without "any later version" clause
Maintainer  : thormichael át gmâïl døt cöm
Stability   : experimental

Predicates and aggregates useful to relational querying. These are not part of the relational algebra but are useful in relational queries in restriction predicates, and extension and summarization expressions. It also has great utility in non-relational queries, such as when performing aggregations.
-}

module Database.HaskRel.Support
       ( -- * Predicates
         (&=), plusminus, (+-), (±),
         between, (>=<=), (≥≤), xBetween, (><), (>=<), (≥<), (><=), (>≤),
         (≤), (≥), (≠),
         -- * Aggregates
         avg, minx, maxx )
       where

import Data.List (genericLength)


infix 5 &=

-- | Reverse predicate application. As "Data.Function.&", the reverse function application operator, but restricted to a boolean result.
(&=) :: a -> ( a -> Bool ) -> Bool
(&=) x f = f x

{-
-- TODO: Perhaps congruence (or a variation thereof?) is useful?
(≅) :: Eq a => t -> t -> (t -> a) -> Bool
(≅) a b f = f a == f b

>>> 38 ≅ 58 $ ( `mod` 10 )
True
-}


-- |>>> plusminus 9 3 10
--True
plusminus :: (Ord a, Num a) => a -> a -> a -> Bool
plusminus x y c = c >= ( x - y ) && c <= ( x + y )

infix 7 ±
infix 7 +-

{-| >>> 10 &= 9 ± 3
True
>>> rPrint$ p ∣ (\[pun|weight|] -> weight &= 9 ± 3)
┌─────┬───────┬───────┬────────┬────────┐
│ pno │ pName │ color │ weight │ city   │
╞═════╪═══════╪═══════╪════════╪════════╡
│ P1  │ Nut   │ Red   │ 12 % 1 │ London │
│ P5  │ Cam   │ Blue  │ 12 % 1 │ Paris  │
└─────┴───────┴───────┴────────┴────────┘
-}
(±) :: (Ord a, Num a) => a -> a -> a -> Bool
(±) = plusminus

-- |>>> 10 &= 9 +- 3
--True
(+-) :: (Ord a, Num a) => a -> a -> a -> Bool
(+-) = plusminus


-- |>>> 5 `between` (5,9)
--True
between :: Ord a => a -> (a, a) -> Bool
between c (l,h) = c >= l && c <= h


{-| >>> 5 &= 5 >=<= 9
True
-}
(>=<=) :: Ord a => a -> a -> a -> Bool
(>=<=) l h c = c >= l && c <= h

{-| >>> 5 &= 5 ≥≤ 9
True
>>> rPrint$ p ∣ (\[pun|weight|] -> weight &= 11 ≥≤ 14)
┌─────┬───────┬───────┬────────┬────────┐
│ pno │ pName │ color │ weight │ city   │
╞═════╪═══════╪═══════╪════════╪════════╡
│ P1  │ Nut   │ Red   │ 12 % 1 │ London │
│ P4  │ Screw │ Red   │ 14 % 1 │ London │
│ P5  │ Cam   │ Blue  │ 12 % 1 │ Paris  │
└─────┴───────┴───────┴────────┴────────┘
-}
(≥≤) :: Ord a => a -> a -> a -> Bool
(≥≤) l h c = c >= l && c <= h

{-|
Between exclusive

>>> 5 `xBetween` (5,9)
False
-}
xBetween :: Ord a => a -> (a, a) -> Bool
xBetween c (l,h) = c > l && c < h

{-| >>> 5 &= 5 >< 9
False
>>> rPrint$ p ∣ (\[pun|weight|] -> weight &= 11 >< 14)
┌─────┬───────┬───────┬────────┬────────┐
│ pno │ pName │ color │ weight │ city   │
╞═════╪═══════╪═══════╪════════╪════════╡
│ P1  │ Nut   │ Red   │ 12 % 1 │ London │
│ P5  │ Cam   │ Blue  │ 12 % 1 │ Paris  │
└─────┴───────┴───────┴────────┴────────┘
-}
(><) :: Ord a => a -> a -> a -> Bool
(><) l h c = c > l && c < h

-- |>>> 5 &= 5 >=< 9
--True
(>=<) :: Ord a => a -> a -> a -> Bool
(>=<) l h c = c >= l && c < h

-- |>>> 5 &= 5 ≥< 9
--True
(≥<) :: Ord a => a -> a -> a -> Bool
(≥<) l h c = c >= l && c < h

-- |>>> 5 &= 5 ><= 9
--False
(><=) :: Ord a => a -> a -> a -> Bool
(><=) l h c = c > l && c <= h

-- |>>> 5 &= 5 >≤ 9
--False
(>≤) :: Ord a => a -> a -> a -> Bool
(>≤) l h c = c > l && c <= h

-- | Synonym for <=
(≤) :: Ord a => a -> a -> Bool
(≤) x y = x <= y

-- | Synonym for >=
(≥) :: Ord a => a -> a -> Bool
(≥) x y = x >= y

-- | Synonym for /=
(≠) :: Eq a => a -> a -> Bool
(≠) x y = x /= y


{-| Average of a list of values

>>> let _qtys a = Label .=. a :: Tagged "qtys" Double
>>> pt$ group sp (rHdr (pno,qty)) (_qtys . avg . agg qty)
┌────────────────────┬───────────────┐
│ qtys :: Double     │ sno :: String │
╞════════════════════╪═══════════════╡
│ 200.0              │ S3            │
│ 216.66666666666666 │ S1            │
│ 300.0              │ S4            │
│ 350.0              │ S2            │
└────────────────────┴───────────────┘

Note the explicit type in the definition of @_qtys@; it is neccessary to provide a specific type for 'pt' to format it as a table, even when an expression would otherwise be correct without this. The following would blow up if prefixed with "pt$":

>>> group sp (rHdr (pno,qty)) ((qtys .=.) . avg . agg qty)
fromList [Record{qtys=200.0,sno="S3"},Record{qtys=216.66666666666666,sno="S1"},Record{qtys=300.0,sno="S4"},Record{qtys=350.0,sno="S2"}]
-}
avg :: (Fractional a, Real a1) => [a1] -> a
avg xs = realToFrac (sum xs) / genericLength xs


{- | Minimum of several values, defaulting to the second argument if there are no elements.

>>> pt$ group sp (rHdr (pno,qty)) ((qtys .=.) . (`minx` 0) . agg qty)
┌─────────────────┬───────────────┐
│ qtys :: Integer │ sno :: String │
╞═════════════════╪═══════════════╡
│ 100             │ S1            │
│ 200             │ S3            │
│ 200             │ S4            │
│ 300             │ S2            │
└─────────────────┴───────────────┘
-}
minx :: Ord a => [a] -> a -> a
minx [] d = d
minx xs _ = minimum xs

{- Or perhaps with Foldable:
minx :: (Ord a, Foldable t) => t a -> a -> a
minx fl d = if F.null fl then d
                         else F.minimum fl
-}

{- | Maximum of several values, defaulting to the second argument if there are no elements

>>> pt$ group sp (rHdr (pno,qty)) ((qtys .=.) . (`maxx` 0) . agg qty)
┌─────────────────┬───────────────┐
│ qtys :: Integer │ sno :: String │
╞═════════════════╪═══════════════╡
│ 200             │ S3            │
│ 400             │ S1            │
│ 400             │ S2            │
│ 400             │ S4            │
└─────────────────┴───────────────┘
-}
maxx :: Ord a => [a] -> a -> a
maxx [] d = d
maxx xs _ = maximum xs
