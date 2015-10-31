{-# LANGUAGE TypeFamilies, FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-} -- For relational assignment

-- TODO! All operators that take more than one relation and do not build upon Expression will need to be relRearranged

{-|
Module      : Database.HaskRelTIP.Relational.Unicode
Description : Functions pertaining to relational theory or set theory using unicode characters
Copyright   : © Thor Michael Støre, 2015
License     : GPL v2 without "any later version" clause
Maintainer  : thormichael át gmail døt com
Stability   : experimental

Functions pertaining to relational theory or set theory using unicode characters, primarily infix operators. These are all synonyms, each of them are based on a prefix function variation.

See also: http://hackage.haskell.org/package/base-unicode-symbols

TODO: Rename to Unicode
-}

module Database.HaskRelTIP.Relational.Unicode (
  -- * Relational algebra operators
  (⋈), (⋉), (⋊), (◅), (▻), (×), (⊠), (∣), (∪), (∩), (∖), π,
  (⊂), (⊆), (⊃), (⊇), 
  -- * Relational assignment operators
  (≔),
  -- * Set theoretic operators
  (#), (∈), (∉), (∋), (∌)
) where

import Database.HaskRelTIP.Relational.Definition ( Relation', RDyadic, HUnwrap, bodyAsList )
import Database.HaskRelTIP.Relational.Expression
import Database.HaskRelTIP.Relational.Variable ( Relvar, writeRelvarBody' )

import Data.Set ( Set )
import Data.HList.CommonMain

-- | Natural join.
r1 ⋈ r2 = naturalJoin r1 r2

-- | Semijoin.
r1 ⋉ r2 = semiJoin r1 r2

-- | Left semijoin.
r1 ⋊ r2 = leftSemiJoin r1 r2


-- | Semidifference.
r1 ◅ r2 = notMatching r1 r2

-- | Left semidifference.
r1 ▻ r2 = leftNotMatching r1 r2

-- | Times. The special case of natural join where the headings of the relations are disjoint.
l × r = times l r

{-| Attribute intersecting natural join. The special case of natural join where the headings of the relations are not disjoint.

Using the "box times" or "squared times" (U+22A0) symbol is my own solution.
As with the name "(attribute) intersecting natural join" suggestions are welcome.

As mention in Rel.hs, this operation doesn't have a single identity value,
although it holds that for any given relation value @r@, @r ⊠ r = r@
-}
l ⊠ r = interJoin l r


{-| Restriction.

Note that the symbol used here is the divisor symbol, which looks the same but is
distinct from the vertical bar, or pipe. However, since the vertical bar is used in Haskell
for different purposes and is for that reason not a valid infix operator
symbol, this is used instead.
-}
(∣) :: (MonOp a, MonOpArg a ~ Set a1) =>
     a -> (a1 -> Bool) -> MonOp' a (Set a1)
r ∣ p = restrict r p

-- | Union.
(∪) :: (Ord (HList l), RDyadic ta l a1 a2, DyaOp a b,
      DyaOpRight b ~ Set (TIP ta), DyaOpLeft a ~ Set (TIP l)) =>
     a -> b -> DyaOp' a b (Relation' l)
l ∪ r = union l r

-- | Intersection.
(∩) :: (Ord (HList l), RDyadic ta l a1 a2, DyaOp a b,
      DyaOpRight b ~ Set (TIP ta), DyaOpLeft a ~ Set (TIP l)) =>
     a -> b -> DyaOp' a b (Relation' l)
l ∩ r = intersect l r

-- | Minus.
(∖) :: (Ord (HList l), RDyadic ta l a1 a2, DyaOp a b,
      DyaOpRight b ~ Set (TIP ta), DyaOpLeft a ~ Set (TIP l)) =>
     a -> b -> DyaOp' a b (Relation' l)
l ∖ r = minus l r

{- | Projection.

Note that no matter how greek it is π is still a character, and
Haskell therefore treats it as a prefix operator, unlike symbols that become
infix operators. This works out quite well though, since it is a prefix
operator when used to denote relational projection.
-}
π :: (Ord (HList l), HAllTaggedEq l, HRLabelSet l,
      H2ProjectByLabels (LabelsOf l1) t l b, MonOp a,
      MonOpArg a ~ Set (TIP t)) =>
     hlistOrRecord l1 -> a -> MonOp' a (Relation' l)
π a r  = project r a

infix 1 ≔

{-|
Relational assignment operator.

This uses the COLON EQUALS UTF-8 character (\&\#8788;), the ASCII variant := wouldn't be
allowed in Haskell since it starts with a colon.

TODO: Support assigning one relvar to another.
-}
(≔) :: forall a b ta .
    (Ord (HList b), Show (HList b), TagUntagFD a ta, 
     HMapCxt HList HUnwrap a b) =>
    Relvar ta -> Relation' ta -> IO ()
(≔) rv r = writeRelvarBody' rv ( bodyAsList r :: [HList b] )
--(≔) rv r = assign' rv r ( undefined :: [HList b] )


-- |>>>fbbRelU ⊂ fbbRel
--False
(⊂) :: (Ord (HList l), RDyadic ta l a1 a2, DyaOp a b,
      DyaOpRight b ~ Set (TIP ta), DyaOpLeft a ~ Set (TIP l)) =>
     a -> b -> DyaOp' a b Bool
l ⊂ r = isProperSubsetOf l r

-- |>>>fbbRelU ⊆ fbbRel
--False
(⊆) :: (Ord (HList l), RDyadic ta l a1 a2, DyaOp a b,
      DyaOpRight b ~ Set (TIP ta), DyaOpLeft a ~ Set (TIP l)) =>
     a -> b -> DyaOp' a b Bool
l ⊆ r = isSubsetOf l r

-- |>>>fbbRelU ⊃ fbbRel
--True
(⊃) :: (Ord (HList l), RDyadic ta l a1 a2, DyaOp a b,
      DyaOpRight b ~ Set (TIP ta), DyaOpLeft a ~ Set (TIP l)) =>
     b -> a -> DyaOp' a b Bool
l ⊃ r = isProperSubsetOf r l

-- |>>>fbbRelU ⊇ fbbRel
--True
(⊇) :: (Ord (HList l), RDyadic ta l a a1) =>
     Relation' ta -> Relation' l -> Bool
l ⊇ r = isSubsetOf r l

-- |>>> (#) fbbRelU
--3
(#) :: (MonOp a, MonOpArg a ~ Set a1) => a -> MonOp' a Int
(#) = count


-- |>>> rTuple' (7,"werg","hotf") ∈ fbbRelU
--True
(∈) :: (Ord a1, MonOp a, MonOpArg a ~ Set a1) =>
     a1 -> a -> MonOp' a Bool
(∈) = member

-- |>>> rTuple' (9,"things","stuff") ∉ fbbRelU
--True
(∉)
  :: (Ord a1, MonOp a, MonOpArg a ~ Set a1) =>
     a1 -> a -> MonOp' a Bool
(∉) = notMember

-- |>>> fbbRelU ∋ rTuple' (9,"things","stuff")
--False
(∋) :: (Ord a1, MonOp a, MonOpArg a ~ Set a1) =>
     a -> a1 -> MonOp' a Bool
r ∋ e = member e r

-- |>>> fbbRelU ∌ rTuple' (7,"werg","hotf")
--False
(∌) :: (Ord a1, MonOp a, MonOpArg a ~ Set a1) =>
     a -> a1 -> MonOp' a Bool
r ∌ e = notMember e r



{-
Other infix operators
∖ minus/relative complement/set-theoretic difference - don't confuse with blackslash - \ - you may need to
  use a different font to see the difference.
 # cardinality - prefix, needs parenthesis
∅ empty set
∆ or ⊖ symmetric difference
π relational projection
◅ ▻ antijoin
σ selection
‼ image relation
:= or ≔ relational assignment

Predicate logic:
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

This would be interesting to find a need for:
ℵ

See also:
http://en.wikipedia.org/wiki/List_of_mathematical_symbols
http://www.fileformat.info/info/unicode/block/mathematical_operators/images.htm
-}
          
