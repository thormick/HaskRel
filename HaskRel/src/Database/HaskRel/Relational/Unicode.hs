{-# LANGUAGE TypeFamilies, FlexibleContexts, ScopedTypeVariables #-}

{-|
Module      : Database.HaskRel.Relational.Unicode
Description : Functions pertaining to relational theory or set theory named with
              unicode characters
Copyright   : © Thor Michael Støre, 2015
License     : GPL v2 without "any later version" clause
Maintainer  : thormichael át gmail døt com
Stability   : experimental

Functions pertaining to relational theory or set theory named with non-ASCII
unicode characters, primarily infix operators. Each of these is a synonym for an
alphabetically named prefix function.

See also: http://hackage.haskell.org/package/base-unicode-symbols

TODO: Fix operator precedence, right now one may need to apply more parenthesis
than should be necessary.
-}

module Database.HaskRel.Relational.Unicode (
  -- * Relational algebra operators
  (⋈), (⋉), (⋊), (◅), (▻), (×), (∣), (∪), (∩), (∖), π,
  (⊂), (⊆), (⊃), (⊇), 
  -- * Relational assignment operators
  (≔),
  -- * Supplementary relational operators
  -- **** Not part of relational theory
  (⊠), 
  -- * Set theoretic operators
  (#), (∈), (∉), (∋), (∌), (∅)
) where

import Database.HaskRel.Relational.Definition ( Relation, bodyAsList, empty )
import Database.HaskRel.Relational.Expression
import Database.HaskRel.Relational.Variable ( Relvar, writeRelvarBody' )


import Data.Set ( Set )
import Data.HList.CommonMain

{-| Natural join.

>>> rPrint$ sp ⋈ s
...

As 'Database.HaskRel.Relational.Expression.naturalJoin'.
-}
r1 ⋈ r2 = naturalJoin r1 r2

{-| Semijoin.

>>> rPrint$ s ⋉ sp
...

As 'Database.HaskRel.Relational.Expression.matching'.
-}
r1 ⋉ r2 = semiJoin r1 r2

-- | Left semijoin. As @(flip 'Database.HaskRel.Relational.Expression.matching')@.
r1 ⋊ r2 = semiJoin r2 r1


{-| Semidifference.

>>> rPrint$ s ◅ sp
...

As 'Database.HaskRel.Relational.Expression.notMatching'.
-}
r1 ◅ r2 = notMatching r1 r2

-- | Left semidifference. As @(flip 'Database.HaskRel.Relational.Expression.notMatching')@.
r1 ▻ r2 = notMatching r2 r1

{-| Times. The special case of natural join where the headings of the relations
are disjoint.

>>> rPrint$ ( sp `projectAllBut` (rHdr (sno)) ) × ( s `projectAllBut` (rHdr (sno)) )
...

As 'Database.HaskRel.Relational.Expression.times'.
-}
l × r = times l r

{-| Attribute intersecting natural join. The special case of natural join where
the headings of the relations are not disjoint.

Using the "box times" or "squared times" (U+22A0) symbol is my own solution. As
with the name "(attribute) intersecting natural join" suggestions are welcome.

As mentioned in "Database.HaskRel.Relational.Algebra", this operation doesn't
have a single identity value, although it holds that for any given relation
value @r@, @r ⊠ r = r@

>>> rPrint$ sp ⊠ s
...

As 'Database.HaskRel.Relational.Expression.interJoin'.
-}
l ⊠ r = interJoin l r


{-| Restriction.

Note that the symbol used here is the divisor symbol, which looks the same but
is distinct from the vertical bar, or pipe. However, since the vertical bar is
used in Haskell for different purposes and is for that reason not a valid infix
operator symbol, this is used instead.

>>> rPrint$ p ∣ (\[pun|weight|] -> weight < 17.5)
...

As 'Database.HaskRel.Relational.Expression.restrict'.
-}
r ∣ p = restrict r p

{-| Union.

>>> rPrint$ s ∪ ( relation [rTuple (sno .=. "S6", sName .=. "Nena", status .=. 40, city .=. "Berlin")] )
...

As 'Database.HaskRel.Relational.Expression.union'.
-}
l ∪ r = l `union` r

{-| Intersection.

>>> let sX = ( relation [rTuple (sno .=. "S2", sName .=. "Jones", status .=. 10, city .=. "Paris"), rTuple (sno .=. "S6", sName .=. "Nena", status .=. 40, city .=. "Berlin")] )
>>> rPrint$ s ∩ sX
...

As 'Database.HaskRel.Relational.Expression.intersect'.
-}
l ∩ r = l `intersect` r

{-| Minus.

Note that this is the difference symbol, not a backslash.

>>> rPrint$ s ∖ sX
...

As 'Database.HaskRel.Relational.Expression.minus'.
-}
l ∖ r = minus l r

{-| Projection.

Note that no matter how greek it is π is still a character, and Haskell
therefore treats it as a prefix operator, which is in line with how it is
employed.

>>> rPrint$ π (rHdr (color,city)) p
...

As 'Database.HaskRel.Relational.Expression.project', but note how the operands
are reversed.
-}
π a r  = project r a

infix 1 ≔

{-|
Relational assignment operator.

This uses the COLON EQUALS UTF-8 character (\&\#8788;), the ASCII variant @ := @
wouldn't be allowed in Haskell since it starts with a colon.

>>> s ≔ s'
>>>

As 'Database.HaskRel.Relational.Expression.assign'.
-}
(≔) rv r = assign rv r


{-| Is proper subset.

>>> let spX = relation [rTuple (sno .=. "S1", pno .=. "P4", qty .=. 200), rTuple (sno .=. "S2", pno .=. "P2", qty .=. 400)]
>>> spX ⊂ sp
True

As 'Database.HaskRel.Relational.Expression.isProperSubsetOf'.
-}
l ⊂ r = isProperSubsetOf l r

{-| Is subset of.

>>> spX ⊆ sp
True

As 'Database.HaskRel.Relational.Expression.isSubsetOf'.
-}
l ⊆ r = isSubsetOf l r

-- | Left is proper subset of. As @(flip 'Database.HaskRel.Relational.Expression.isProperSubsetOf')@.
l ⊃ r = isProperSubsetOf r l

-- | Left is subset of. As @(flip 'Database.HaskRel.Relational.Expression.isSubsetOf')@.
l ⊇ r = isSubsetOf r l

{-| Cardinality.

>>> (#) sp
12

As 'Database.HaskRel.Relational.Expression.count'.
-}
(#) r = count r


{-| Is member of.

>>> let spX = rTuple(sno .=. "S3", qty .=. 200, pno .=. "P2")
>>> spX ∈ sp
True

As 'Database.HaskRel.Relational.Expression.member'.
-}
l ∈ r = member l r

{-| Is not member of.

>>> spX ∉ sp
False

As 'Database.HaskRel.Relational.Expression.notMember'.
-}
l ∉ r = notMember l r

-- | Is member of left. As @(flip 'Database.HaskRel.Relational.Expression.member')@.
r ∋ e = member e r

-- | Is not member of left. As @(flip 'Database.HaskRel.Relational.Expression.notMember')@.
r ∌ e = notMember e r

{- | The empty set. Note that this is neither 'tableDum' nor 'tableDee'.

>>> (relation [] :: Relation '[]) == (∅)
True
>>> relation [rTuple (sno .=. "S1", status .=. 5)] == (∅)
False
-}
(∅) = empty

{-
Other infix operators
∖ minus/relative complement/set-theoretic difference - don't confuse with blackslash - \ - you may need to use a different font to see the difference.
 # cardinality - prefix, needs parenthesis
∅ empty set
∆ or ⊖ symmetric difference
π relational projection
◅ ▻ antijoin
σ selection
‼ image relation
:= or ≔ relational assignment

Predicate logic, although the Haskell library http://hackage.haskell.org/package/base-unicode-symbols already uses them:
∀ Forall
∃ Exists

Other common and useful operators of arithmetry and whatnot:
≤
≥
±
∑
≬ between
≺ preceeds
≻ suceeds

See also:
http://en.wikipedia.org/wiki/List_of_mathematical_symbols
http://www.fileformat.info/info/unicode/block/mathematical_operators/images.htm
-}
          
