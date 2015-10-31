{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, DataKinds #-}
{-# LANGUAGE UndecidableInstances, TypeOperators #-}

{- DataKinds is typically required by users of this module, including on GHCi
   (:set -XDataKinds), ViewPatterns is also required in order to use certain
   constructs, and AutoDeriveTypeable can also be beneficial. An
   "AutoDeriveNewtypeData" would also have been convenient.
-}

{-|
Module      : Database.HaskRelTIP.Relational.Algebra
Description : TIP based relational algebra, including supporting functions
Copyright   : © Thor Michael Støre, 2015
License     : GPL v2 without "any later version" clause
Maintainer  : thormichael át gmail døt com
Stability   : experimental

Relational algebra based on HList TIPs.

It is important to note in this module that these functions are defined such that they only operate upon relational /values/, not relvars or the result of expressions involving relvars. See `Database.HaskRelTIP.Relational.Expression` for functions that function as conveyed by the relational model, the ones that are intended to be used directly, although the functions as defined in this module have more readily understandable type signatures.

As such, all the examples here are defined in terms of the relation values s', p' and sp', instead of their relvar counterparts. The examples can be run directly in the GHCi session started by examples\/TIP\/algebraExample.sh. The equivalent commands with relvars s, p and sp can be run with the session started by examples\/TIP\/SuppliersPartsExample.hs, which loads `Database.HaskRelTIP.Relational.Expression` instead.
-}
module Database.HaskRelTIP.Relational.Algebra (
  -- * Operators of the relational algebra
  rename, extend, restrict, project, projectAllBut,
  projectR,
  naturalJoin, nJoin, matching, semiJoin, notMatching, semiDiff, times,
  union, dUnion, intersect, minus, xUnion, xMinus, group, groupAllBut, ungroup,
  -- * Additional operators with relational closure
  interJoin, iJoin,
{- | Extend strictly. Extends the given relation with the attribute returned by the second argument, without deleting it if it exists. @l@ cannot already have an argument @e@.
-}
  dExtend,
  -- * Sugared variations of relational operators
  extend1, extend2, extend3, extend4, extend5,
  restrict1, restrict2, restrict3, restrict4, restrict5,
  -- ** Alternative, concise functions
  rename', as, project', group',
  -- * Support operators/functions
  isProperSubsetOf, isSubsetOf, image, count, isEmpty,
  -- * Other useful associated functions
  rfoldr1, rfoldr2, rfoldr3, rfoldr4, rfoldr5, rafoldr, rafoldr', rafoldrU, agg, agg', aggU,
  relRearrange, joinRight ) where

import Data.Set ( Set, size, fromList, toList, difference, intersection )
import qualified Data.Set

import Data.Data

import Control.Lens.Wrapped ( Wrapped, Unwrapped, _Wrapped' )
import Control.Lens ( (^.) )

import Data.HList.CommonMain

import Database.HaskRelTIP.Relational.Definition
import Database.HaskRelTIP.TIPFWTabulation

{-
*RelExample> pt$ fromList [Foo 1 .*. Bar "zxv" .*. Baz "qwer" .*. emptyTIP, Foo 2 .*. Bar "poiu" .*. Baz "hiufn" .*. emptyTIP]
┌───────────┬──────────────┬──────────────┐
│ Foo : Int │ Bar : String │ Baz : String │
╞═══════════╪══════════════╪══════════════╡
│ 1         │ "zxv"        │ "qwer"       │
│ 2         │ "poiu"       │ "hiufn"      │
└───────────┴──────────────┴──────────────┘
-}

{- | Attribute order rearrangment

Because element order is significant for TIP types in the implementation, they
need to be rearranged  for operations that take two or more relation types as
arguments
-}

-- Induced signature without RDyadic:
--relRearrange :: (Ord (HList l), TagUntagFD ta' ta, TagUntagFD l' l,
--      HProject (HList ta') (HList l')) =>
--     Relation' ta -> Relation' l
relRearrange :: (RDyadic arg res arg' res', Ord (HList res)) =>
     Relation' arg -> Relation' res
relRearrange = Data.Set.map hTIPRearrange

{-| Renames an attribute.

>>> rPrint$ rename s' ( undefined :: City ) ( undefined :: SCity )
┌────────┬─────┬───────┬────────┐
│ SCity  │ SNO │ SName │ Status │
╞════════╪═════╪═══════╪════════╡
│ Athens │ S5  │ Adams │ 30     │
│ London │ S1  │ Smith │ 20     │
│ London │ S4  │ Clark │ 20     │
│ Paris  │ S2  │ Jones │ 10     │
│ Paris  │ S3  │ Blake │ 30     │
└────────┴─────┴───────┴────────┘
-}
rename :: forall r v v' a b l. (Ord (r v'), HOccurs a l, HExtend b l,
      HDeleteAtLabel r a v v', HExtendR b l ~ r v,
      Wrapped a, Wrapped b, Unwrapped a ~ Unwrapped b) =>
     Set l -> a -> b -> Set (r v')
rename r _ _ = Data.Set.map (renameTIPLabel (undefined :: a) (undefined :: b)) r
-- TODO: Fix this so it preserves attribute order. That should simplify the signature too

infix 5 `rename'`

{-| Alternative version of `rename`, see `as` below. -}
rename' :: forall x r v v' a b l. (Ord (r v'), HOccurs a l, HExtend b l,
      HDeleteAtLabel r a v v', HExtendR b l ~ r v,
      Wrapped a, Wrapped b, Unwrapped a ~ Unwrapped b) =>
     Set l -> ((x -> a), (x -> b)) -> Set (r v')
rename' r _ = Data.Set.map (renameTIPLabel (undefined :: a) (undefined :: b)) r

infix 7 `as`

{-| Enables expressing the example for `rename` as:

>>> rPrint$ s' `rename'` ( City `as` SCity )

(Parenthesis optional.)

This is designed to be closer to the Tutorial D syntax (see [1] p. 109):

@ S RENAME { CITY AS SCITY }@
-}
as :: t -> t1 -> (t, t1)
a1 `as` a2 = (a1, a2)


{- | Extends the given relation with the attribute returned by the second argument. If an attribute of the type given by the second argument already exists in the first argument then it will be replaced.

>>> rPrint$ extend p' (\(hOccurs -> (Weight weight)) -> GMWT $ weight * 454)
┌──────────┬─────┬───────┬───────┬────────┬────────┐
│ GMWT     │ PNO │ PName │ Color │ Weight │ City   │
╞══════════╪═════╪═══════╪═══════╪════════╪════════╡
│ 5448 % 1 │ P1  │ Nut   │ Red   │ 12 % 1 │ London │
...
│ 8626 % 1 │ P6  │ Cog   │ Red   │ 19 % 1 │ London │
└──────────┴─────┴───────┴───────┴────────┴────────┘
>>> rPrint$ extend p' (\(hOccurs -> (Weight weight)) -> Weight $ weight + 10)
┌────────┬─────┬───────┬───────┬────────┐
│ Weight │ PNO │ PName │ Color │ City   │
╞════════╪═════╪═══════╪═══════╪════════╡
│ 22 % 1 │ P1  │ Nut   │ Red   │ London │
...

See also `extend1` et. al. for versions sugared with `hOccurs`\/@tipyTuple/n/@.
-}
-- TODO: Support both extending by an attribute and an r-tuple
-- TODO: Instead of delete-and-append, would it be possible to replace it in-line? Perhaps a class with an instance for the case where the element exists and another for where it doesn't, which would make it possible to use ttip in the former case. This would also solve the issue with how an existing element gets moved around, which is probably undesireable in the majority of cases.
-- TODO: With a bit of type family trickery it should be possible to define an extend function that operates upon both sets of records and sets of TIPS. Not sure if this is worthwhile.
extend :: forall e r v v'.
     (Ord (HExtendR e (r v')), HExtend e (r v'),
      HDeleteAtLabel r e v v') =>
     Set (r v) -> (r v -> e) -> Set (HExtendR e (r v'))
extend r f = Data.Set.map (\t -> ( f t ) .*. ( hDeleteAtLabel (Label::Label e) t ) ) r

{- | Disjoint extend. Extends the given relation with the attribute returned by the second argument, without deleting it if it exists. @l@ cannot already have an argument @e@.
-}
dExtend :: (Ord (HExtendR e l), HExtend e l) =>
     Set l -> (l -> e) -> Set (HExtendR e l)
dExtend r f = Data.Set.map (\t -> ( f t ) .*. t ) r

{-
*Rel> :set -XViewPatterns
*Rel> :set -XDataKinds
*Rel> pt$ extend (relation' [( 1, "asdf", "zcxv" ), ( 2, "xzvc", "qewr" )] :: Relation '[Foo,Bar,Baz]) (\( unTagTIP -> Foo foo `HCons` ( Bar bar `HCons` ( Baz baz `HCons` HNil ) ) ) -> Qux $ show foo ++ ": " ++ baz ++ ", " ++ bar )
┌─────────────────┬───────────┬──────────────┬──────────────┐
│ Qux : String    │ Foo : Int │ Bar : String │ Baz : String │
├─────────────────┼───────────┼──────────────┼──────────────┤
│ "1: zcxv, asdf" │ 1         │ "asdf"       │ "zcxv"       │
│ "2: qewr, xzvc" │ 2         │ "xzvc"       │ "qewr"       │
└─────────────────┴───────────┴──────────────┴──────────────┘
The elements of argument 'a' of extend must be in order, which is rather
unfortunate given that we shouldn't be burdened with such things when working on
relational theory.
-}

{-
Ideally the rather convoluted pattern matching with nested HCons will be hidden
behind a DSL, but when doing this directly in Haskell, and particularly on GHCi,
a syntactically more pleasing variation is desireable. It is possible to do:

*Rel> pt$ extend (relation' [( 1, "asdf", "zcxv" ), ( 2, "xzvc", "qewr" )] :: Relation '[Foo,Bar,Baz]) (\( tipyTuple3 -> ( Foo foo, Bar bar, Baz baz ) ) -> Qux $ show foo ++ ": " ++ baz ++ ", " ++ bar )

Therefore:
-}

{-|
>>> rPrint$ extend1 p' (\(Weight weight) -> GMWT $ weight * 454)

Result as with the example for `extend`.
-}
extend1
  :: (Ord (HExtendR e (r v')), HOccurs b (r v), HExtend e (r v'),
      HDeleteAtLabel r e v v') =>
     Set (r v) -> (b -> e) -> Set (HExtendR e (r v'))
extend1 r f = extend r ( f . hOccurs )
extend2 r f = extend r ( f . tipyTuple )
extend3 r f = extend r ( f . tipyTuple3 )
extend4 r f = extend r ( f . tipyTuple4 )
extend5 r f = extend r ( f . tipyTuple5 )

{- Which means we can also do:
*Rel> :set -XDataKinds
*Rel> pt$ extend2 (relation' [( 1, "asdf", "zcxv" ), ( 2, "xzvc", "qewr" )] :: Relation '[Foo,Bar,Baz]) (\( Bar bar, Baz baz ) -> Qux $ baz ++ ", " ++ bar )
┌──────────────┬───────────┬──────────────┬──────────────┐
│ Qux : String │ Foo : Int │ Bar : String │ Baz : String │
├──────────────┼───────────┼──────────────┼──────────────┤
│ "qewr, xzvc" │ 2         │ "xzvc"       │ "qewr"       │
│ "zcxv, asdf" │ 1         │ "asdf"       │ "zcxv"       │
└──────────────┴───────────┴──────────────┴──────────────┘
Here the arguments don't have to be specified in correct order,
"( Bar bar, Foo foo )" would work just as well in the previous example.


I'm not satisfied with this though, the limiting factor becomes Haskell's tuples, but they're only a means to an end here. All that is required is to establish a context with variables named the same as the attributes, and having to specify the number of variables like this is rather silly. It is also a major issue that such a scope should contain both outer attributes within a nested context, as well as relation values themselves. This is also an issue with restriction.
-}

{- |
Restricts the given relation according to the given predicate. Note that this is the well-known `WHERE` operator of both SQL and Tutorial D, but since "where" is a reserved keyword in Haskell it is named "restrict".

>>> rPrint$ p' `restrict` (\( hOccurs -> (Weight weight) ) -> weight < 17.5 )
┌─────┬───────┬───────┬────────┬────────┐
│ PNO │ PName │ Color │ Weight │ City   │
╞═════╪═══════╪═══════╪════════╪════════╡
│ P1  │ Nut   │ Red   │ 12 % 1 │ London │
...
│ P5  │ Cam   │ Blue  │ 12 % 1 │ Paris  │
└─────┴───────┴───────┴────────┴────────┘

See also `restrict1` et. al. for versions sugared with `hOccurs`\/@tipyTuple/n/@.
-}
restrict :: Set a -> (a -> Bool) -> Set a
restrict r f = Data.Set.filter f r

{-| Sugared version of `restrict`, as with `extend`.

>>> rPrint$ p' `restrict1` (\( Weight weight ) -> weight < 17.5 )

Result as with the example for `restrict`.
-}
restrict1 :: HOccurs b l => Set l -> (b -> Bool) -> Set l
restrict1 r f = Data.Set.filter ( f . hOccurs ) r
restrict2 r f = Data.Set.filter ( f . tipyTuple ) r
restrict3 r f = Data.Set.filter ( f . tipyTuple3 ) r
restrict4 r f = Data.Set.filter ( f . tipyTuple4 ) r
restrict5 r f = Data.Set.filter ( f . tipyTuple5 ) r

{- Here's a fun & fruitless exercise:
import GHC.Prim

class RestrictN a r t where
  restrictN :: Set r -> a -> (t -> Bool) -> Set r

-- restrictN r () (\() -> True)
instance RestrictN () r () where
  restrictN r _ p = Data.Set.filter (\_ -> p ()) r

-- restrictN r (PNO) (\pno -> pno == "foo")
instance HOccurs t1 r => RestrictN (x1 -> t1) r x1 where
  restrictN r _ p = Data.Set.filter (\t -> p $ ( coerce $ hOccurs t ( undefined :: t1 ) :: x1 ) ) r

Doesn't work, because type variable ‘x1’ would escape its scope

-- restrictN r (PNO,SNO) (\pno sno -> pno == "foo" && sno == "bar")
...
-}

{-| Projects the given relation on the given header.

>>> rPrint$ p' `project` (undefined :: RHdr '[Color, City])
┌───────┬────────┐
│ Color │ City   │
╞═══════╪════════╡
│ Blue  │ Oslo   │
│ Blue  │ Paris  │
│ Green │ Paris  │
│ Red   │ London │
└───────┴────────┘

NOTE: This variant describes the *tuple* and not the *relation* type! Note how "RHdr" is used instead of "Relation"! (TODO: Fix that, get image relations working with a projectAllBut variation that uses that too)
TODO: Should fail if there are labels that do not exist in the value being projected.
-}
project ::
     (Ord (HList l), HAllTaggedEq l, HRLabelSet l,
      H2ProjectByLabels (LabelsOf l1) t l b) =>
     Relation' t -> hlistOrRecord l1 -> Relation' l
project r a = Data.Set.map ( tipyProject $ labelsOf a ) r

{-| Projects the given relation on the header on said given relation minus the given header.

>>> rPrint$ p' `projectAllBut` (undefined :: RHdr '[City])
┌─────┬───────┬───────┬────────┐
│ PNO │ PName │ Color │ Weight │
╞═════╪═══════╪═══════╪════════╡
│ P1  │ Nut   │ Red   │ 12 % 1 │
...
│ P6  │ Cog   │ Red   │ 19 % 1 │
└─────┴───────┴───────┴────────┘
-}
projectAllBut ::
     (Ord (HList l1), HAllTaggedEq l, HAllTaggedEq l1, HRLabelSet l,
      HRLabelSet l1, H2ProjectByLabels (LabelsOf l2) r l l1) =>
     Relation' r -> hlistOrRecord l2 -> Relation' l1
projectAllBut r a = Data.Set.map ( snd . ( tipyProject2 $ labelsOf a ) ) r
-- TODO: Find a cleaner approach than snd . tipyProject2

-- | Variation of project that takes a relation type as the second argument. TODO: Resolve what the default should be defined, probably a RelHdr type.
projectR :: forall l l1 t b hlistOrRecord .
     (Ord (HList l), HAllTaggedEq l, HRLabelSet l,
      H2ProjectByLabels (LabelsOf l1) t l b) =>
     Relation' t -> Set ( hlistOrRecord l1 ) -> Relation' l
projectR r _ = Data.Set.map ( tipyProject $ labelsOf ( undefined :: hlistOrRecord l1 ) ) r




{-| A more concise version of `project`. Comparable to `rename'` with the additional limit that it can only project into a relation of at most 5 degrees.

>>> rPrint$ p' `project'` (Color, City)

Result as with example for @project@.
-}
project' :: (Ord (HList l), HAllTaggedEq l, HLabelSet (LabelsOf l),
      H2ProjectByLabels (LabelsOf l1) t l b, HAllTaggedLV l,
      RHdrClass a (hlistOrRecord l1)) =>
     Relation' t -> a -> Relation' l
project' r a = project r ( rHdr a )

projectAllBut' ::
     (Ord (HList l1), HAllTaggedEq l1, HAllTaggedEq l,
      HLabelSet (LabelsOf l), HLabelSet (LabelsOf l1),
      H2ProjectByLabels (LabelsOf l2) r l l1, HAllTaggedLV l1,
      HAllTaggedLV l, RHdrClass a (hlistOrRecord l2)) =>
     Relation' r -> a -> Relation' l1
projectAllBut' r a = projectAllBut r ( rHdr a )

{-
*Rel> pt $ naturalJoin ( relation [ ( Foo 1, Bar "lkjh", Baz "uhb" ), ( Foo 5, Bar "poiu", Baz "ijn" ), ( Foo 5, Bar "poiu", Baz "okm" ) ] ) ( relation [ ( Foo 1, Qux "asdf" ), ( Foo 1, Qux "qewr") ] )
┌───────────┬──────────────┬──────────────┬──────────────┐
│ Foo : Int │ Bar : String │ Baz : String │ Qux : String │
├───────────┼──────────────┼──────────────┼──────────────┤
│ 1         │ "lkjh"       │ "uhb"        │ "asdf"       │
│ 1         │ "lkjh"       │ "uhb"        │ "qewr"       │
└───────────┴──────────────┴──────────────┴──────────────┘

pt $ naturalJoin ( relation [ ( Foo 1, Bar "lkjh", Baz "uhb" ), ( Foo 5, Bar "poiu", Baz "ijn" ), ( Foo 5, Bar "poiu", Baz "okm" ) ] ) ( relation [ ( Bar "lkjh", Foo 1, Qux "asdf" ), ( Bar "lkjh", Foo 1, Qux "qewr") ] )
... ditto ...

But, with == instead of unordTIPEq:
<interactive>:567:1:
    Couldn't match type ‘TagR (UntagR (UnPrime t0))’ with ‘UnPrime t0’
    The type variable ‘t0’ is ambiguous
    Expected type: Tagged Foo Foo
                     : Tagged Bar Bar : Tagged Baz Baz : UnPrime t0
      Actual type: TagR
                     (UntagR
                        (Tagged Foo Foo : Tagged Bar Bar : Tagged Baz Baz : UnPrime t0))
    In the expression: pt
    ... etc ...
-}

{-| Performs a natural join of the two given relations.

>>> rPrint$ p' `naturalJoin` s'
┌─────┬───────┬───────┬────────┬────────┬─────┬───────┬────────┐
│ PNO │ PName │ Color │ Weight │ City   │ SNO │ SName │ Status │
╞═════╪═══════╪═══════╪════════╪════════╪═════╪═══════╪════════╡
│ P1  │ Nut   │ Red   │ 12 % 1 │ London │ S1  │ Smith │ 20     │
...
│ P6  │ Cog   │ Red   │ 19 % 1 │ London │ S4  │ Clark │ 20     │
└─────┴───────┴───────┴────────┴────────┴─────┴───────┴────────┘
-}
naturalJoin ::
     (RDyadic ta1 ta2 a1 a2, Ord (HList (HAppendListR t1 l1)),
      HAllTaggedEq l1, HAllTaggedEq (HAppendListR t1 l1),
      HRLabelSet l1, HRLabelSet (HAppendListR t1 l1),
      H2ProjectByLabels (LabelsOf t1) t2 ta2 l1,
      H2ProjectByLabels (LabelsOf t2) t1 ta1 b,
      HAppendList t1 l1) =>
     Relation' t1 -> Relation' t2 -> Relation' (HAppendListR t1 l1)
naturalJoin r1 r2 = fromList $ Data.Set.foldr (\t1 b -> joinRight t1 r2 ++ b ) [] r1
-- TODO? Directly build a set instead of building and converting a list? Not sure
-- if it's better to build a set with insert that way. Does the note about
-- performance in Data.Set regarding fromList also apply to insert?
{-
relRearrange :: (Ord (HList res), TagUntagFD arg' arg, TagUntagFD res' res,
      HProject (HList arg') (HList res')) =>
     Relation' arg -> Relation' res
relRearrange :: (Ord (HList res), RDyadic arg res arg' res') =>
     Relation' arg -> Relation' res
-}
-- | Alias of `naturalJoin`.
nJoin
  :: (Eq (HList a2), Ord (HList (HAppendListR t1 l1)),
      TagUntagFD a1 ta1, TagUntagFD a2 ta2, HAllTaggedEq l1,
      HAllTaggedEq ta1, HAllTaggedEq ta2,
      HAllTaggedEq (HAppendListR t1 l1), HLabelSet (LabelsOf ta1),
      HLabelSet (LabelsOf ta2), HLabelSet (LabelsOf l1),
      HLabelSet (LabelsOf (HAppendListR t1 l1)),
      H2ProjectByLabels (LabelsOf t1) t2 ta2 l1,
      H2ProjectByLabels (LabelsOf t2) t1 ta1 b,
      HProject (HList a1) (HList a2), HAppendList t1 l1, HAllTaggedLV l1,
      HAllTaggedLV ta1, HAllTaggedLV ta2,
      HAllTaggedLV (HAppendListR t1 l1)) =>
     Relation' t1 -> Relation' t2 -> Relation' (HAppendListR t1 l1)
nJoin r1 r2 = naturalJoin r1 r2


joinRight
  :: (Eq (HList a), TagUntagFD a ta, TagUntagFD a1 ta1,
      HAllTaggedEq l1, HAllTaggedEq ta, HAllTaggedEq ta1,
      HAllTaggedEq (HAppendListR t l1), HLabelSet (LabelsOf ta),
      HLabelSet (LabelsOf l1), HLabelSet (LabelsOf ta1),
      HLabelSet (LabelsOf (HAppendListR t l1)),
      H2ProjectByLabels (LabelsOf t) r ta l1,
      H2ProjectByLabels (LabelsOf r) t ta1 b,
      HProject (HList a1) (HList a), HAppendList t l1, HAllTaggedLV l1,
      HAllTaggedLV ta, HAllTaggedLV ta1,
      HAllTaggedLV (HAppendListR t l1)) =>
     TIP t -> Set (TIP r) -> [TIP (HAppendListR t l1)]
joinRight t1 r2 = Data.Set.foldr (tupJoin t1) [] r2
  where
    tupJoin t1' t2' b =
      let
        (p',cP) = tipyProject2 ( labelsOf t1' ) t2'
        t2lab = labelsOf t2'
      in
        if p' `unordTIPEq` tipyProject t2lab t1'
        then ( hAppend t1' cP ) : b
        else b


{-| Performs a semi-join of the first given relation against the second given relation.

>>> rPrint$ s' `matching` sp'
┌─────┬───────┬────────┬────────┐
│ SNO │ SName │ Status │ City   │
╞═════╪═══════╪════════╪════════╡
│ S1  │ Smith │ 20     │ London │
│ S2  │ Jones │ 10     │ Paris  │
│ S3  │ Blake │ 30     │ Paris  │
│ S4  │ Clark │ 20     │ London │
└─────┴───────┴────────┴────────┘
-}

matching
  :: (Eq (HList a), Ord (HList t), TagUntagFD a1 ta1,
      TagUntagFD a ta, HAllTaggedEq ta1, HAllTaggedEq ta,
      HLabelSet (LabelsOf ta1), HLabelSet (LabelsOf ta),
      H2ProjectByLabels (LabelsOf t) l ta b,
      H2ProjectByLabels (LabelsOf l) t ta1 b1,
      HProject (HList a1) (HList a), HAllTaggedLV ta1,
      HAllTaggedLV ta) =>
     Relation' t -> Relation' l -> Relation' t
matching = semiJoin

-- | Alias of `matching`
semiJoin ::
     (RDyadic ta1 ta a1 a, Ord (HList t),
      H2ProjectByLabels (LabelsOf t) l ta b,
      H2ProjectByLabels (LabelsOf l) t ta1 b1 ) =>
     Relation' t -> Relation' l -> Relation' t
semiJoin r1 r2 = fromList $ Data.Set.foldr (\t1 b -> semiJoinRight t1 ( toList r2 ) ++ b) [] r1

semiJoinRight
  :: (Eq (HList a), TagUntagFD a ta, TagUntagFD a1 ta1,
      HAllTaggedEq ta, HAllTaggedEq ta1, HLabelSet (LabelsOf ta),
      HLabelSet (LabelsOf ta1), H2ProjectByLabels (LabelsOf t) l ta b,
      H2ProjectByLabels (LabelsOf l) t ta1 b1,
      HProject (HList a1) (HList a), HAllTaggedLV ta,
      HAllTaggedLV ta1) =>
     TIP t -> [TIP l] -> [TIP t]
semiJoinRight _ [] = []
semiJoinRight t1 [t2] =
    if tIntersectEq t1 t2 then [t1]
                          else []
semiJoinRight t1 (t2:r2) =
    if tIntersectEq t1 t2 then [t1]
                          else semiJoinRight t1 r2

-- Equality of intersection of two r-tuples
tIntersectEq
  :: (Eq (HList a), TagUntagFD a ta, TagUntagFD a1 ta1,
      HAllTaggedEq ta, HAllTaggedEq ta1, HLabelSet (LabelsOf ta),
      HLabelSet (LabelsOf ta1), H2ProjectByLabels (LabelsOf t) l ta b,
      H2ProjectByLabels (LabelsOf l) t ta1 b1,
      HProject (HList a1) (HList a), HAllTaggedLV ta,
      HAllTaggedLV ta1) =>
     TIP t -> TIP l -> Bool
tIntersectEq t1 t2 = tipyProject ( labelsOf t1 ) t2 `unordTIPEq` tipyProject ( labelsOf t2 ) t1

{- | Performs a semi-difference of the first given relation against the second given relation.

Aka. antijoin:

/Also known, a trifle inappropriately, as antijoin./

   - Chris Date 2011, SQL and Relational Theory 2nd ed. p. 133

>>> rPrint$ s' `notMatching` sp'
┌─────┬───────┬────────┬────────┐
│ SNO │ SName │ Status │ City   │
╞═════╪═══════╪════════╪════════╡
│ S5  │ Adams │ 30     │ Athens │
└─────┴───────┴────────┴────────┘
-}
notMatching
  :: (Eq (HList a), Ord (HList t), TagUntagFD a1 ta1,
      TagUntagFD a ta, HAllTaggedEq ta1, HAllTaggedEq ta,
      HLabelSet (LabelsOf ta1), HLabelSet (LabelsOf ta),
      H2ProjectByLabels (LabelsOf t) l ta b,
      H2ProjectByLabels (LabelsOf l) t ta1 b1,
      HProject (HList a1) (HList a), HAllTaggedLV ta1,
      HAllTaggedLV ta) =>
     Relation' t -> Relation' l -> Relation' t
notMatching r1 r2 = semiDiff r1 r2

-- | Alias of `notMatching`
semiDiff ::
     (RDyadic ta1 ta a1 a, Ord (HList t),
      H2ProjectByLabels (LabelsOf t) l ta b,
      H2ProjectByLabels (LabelsOf l) t ta1 b1 ) =>
     Relation' t -> Relation' l -> Relation' t
semiDiff r1 r2 = 
    let tl2 = toList r2
        in fromList $ Data.Set.foldr (\t1 b -> semiDiffRight t1 tl2 ++ b) [] r1

semiDiffRight
  :: (Eq (HList a), TagUntagFD a ta, TagUntagFD a1 ta1,
      HAllTaggedEq ta, HAllTaggedEq ta1, HLabelSet (LabelsOf ta),
      HLabelSet (LabelsOf ta1), H2ProjectByLabels (LabelsOf t) l ta b,
      H2ProjectByLabels (LabelsOf l) t ta1 b1,
      HProject (HList a1) (HList a), HAllTaggedLV ta,
      HAllTaggedLV ta1) =>
     TIP t -> [TIP l] -> [TIP t]
semiDiffRight t1 [] = [t1]
semiDiffRight t1 [t2] =
    if tIntersectEq t1 t2 then []
                          else [t1]
semiDiffRight t1 (t2:r2) =
    if tIntersectEq t1 t2 then []
                          else semiDiffRight t1 r2


{-| Performs a natural join between two relations with disjoint headers. A specialized natural join.

>>> rPrint$ ( p' `projectAllBut` (undefined :: RHdr '[City]) ) `times` ( s' `projectAllBut` (undefined :: RHdr '[City]) )
┌─────┬───────┬───────┬────────┬─────┬───────┬────────┐
│ PNO │ PName │ Color │ Weight │ SNO │ SName │ Status │
╞═════╪═══════╪═══════╪════════╪═════╪═══════╪════════╡
│ P1  │ Nut   │ Red   │ 12 % 1 │ S1  │ Smith │ 20     │
│ P1  │ Nut   │ Red   │ 12 % 1 │ S2  │ Jones │ 10     │
...
│ P6  │ Cog   │ Red   │ 19 % 1 │ S4  │ Clark │ 20     │
│ P6  │ Cog   │ Red   │ 19 % 1 │ S5  │ Adams │ 30     │
└─────┴───────┴───────┴────────┴─────┴───────┴────────┘
-}
times ::
     (HRLabelSet (HAppendListR t r),  -- This is added, to ensure that r and l are disjoint.
--      HTIntersect (LabelsOf r) (LabelsOf ta) '[], -- This should also do the job, but the error message isn't as pretty
      RDyadic ta1 ta a1 a, Ord (HList (HAppendListR t l1)), HAllTaggedEq l1,
      HAllTaggedEq (HAppendListR t l1), HRLabelSet l1,
      HRLabelSet (HAppendListR t l1),
      H2ProjectByLabels (LabelsOf t) r ta l1,
      H2ProjectByLabels (LabelsOf r) t ta1 b,
      HAppendList t l1) =>
     Relation' t -> Relation' r -> Relation' (HAppendListR t l1)
times r1 r2 = naturalJoin r1 r2

-- TODO: Get this working, now it just ends up with overlapping error instead of the Fail IsEmpty
data IsEmpty

class NotEmpty ( l :: [*] )
instance NotEmpty l -- Is there any way to match on "anything that is not '[]"?
instance ( Fail IsEmpty ) => NotEmpty '[]

-- Finding out that when : is induced one must simply use ': instead was frustrating

{-| Performs a natural join between two relations with intersecting headers. A specialized natural join.

A join upon relations r1, r2 where the intersection of the heading of r1 and of r2 is not empty; the headings are not disjoint. This is the complement of `times` that together with it forms a natural join; all that would be disallowed for @times@ is allowed here and vice-versa. The name is what I quickly settled on, suggestions for a better one would be welcome. (Attribute-Intersecting Natural Join is another candidate.)

This function doesn't have a specific identity value, although it holds that @r \`interJoin\` r = r@
-}
interJoin ::
     (HTIntersect (LabelsOf r) (LabelsOf t) i, NotEmpty i, -- here i must not be '[]
      RDyadic ta1 ta a1 a, Ord (HList (HAppendListR t l1)), HAllTaggedEq l1,
      HAllTaggedEq (HAppendListR t l1), HRLabelSet l1,
      HRLabelSet (HAppendListR t l1),
      H2ProjectByLabels (LabelsOf t) r ta l1,
      H2ProjectByLabels (LabelsOf r) t ta1 b,
      HAppendList t l1) =>
     Relation' t -> Relation' r -> Relation' (HAppendListR t l1)
interJoin r1 r2 = naturalJoin r1 r2

-- | Alias of `interJoin`
iJoin
  :: (Eq (HList a), Ord (HList (HAppendListR t l1)),
      TagUntagFD a1 ta1, TagUntagFD a ta, HAllTaggedEq l1,
      HAllTaggedEq ta1, HAllTaggedEq ta,
      HAllTaggedEq (HAppendListR t l1), HLabelSet (LabelsOf ta1),
      HLabelSet (LabelsOf ta), HLabelSet (LabelsOf l1),
      HLabelSet (LabelsOf (HAppendListR t l1)),
      H2ProjectByLabels (LabelsOf r) t ta1 b,
      H2ProjectByLabels (LabelsOf t) r ta l1,
      HProject (HList a1) (HList a),
      HTIntersect (LabelsOf r) (LabelsOf t) i, HAppendList t l1,
      HAllTaggedLV l1, HAllTaggedLV ta1, HAllTaggedLV ta,
      HAllTaggedLV (HAppendListR t l1), NotEmpty i) =>
     Relation' t -> Relation' r -> Relation' (HAppendListR t l1)
iJoin = interJoin


{- | Performs a union of the given relations.

>>> rPrint$ s' `union` ( relation [(SNO "S6", SName "Nena", Status 40, City "Berlin")] )
┌─────┬───────┬────────┬────────┐
│ SNO │ SName │ Status │ City   │
╞═════╪═══════╪════════╪════════╡
│ S1  │ Smith │ 20     │ London │
│ S2  │ Jones │ 10     │ Paris  │
│ S3  │ Blake │ 30     │ Paris  │
│ S4  │ Clark │ 20     │ London │
│ S5  │ Adams │ 30     │ Athens │
│ S6  │ Nena  │ 40     │ Berlin │
└─────┴───────┴────────┴────────┘
-}
union ::
     (RDyadic ta l a a1, Ord (HList l) ) =>
     Relation' l -> Relation' ta -> Relation' l
union l r = Data.Set.union l $ relRearrange r

{-| Performs a disjoint union between the two relvars. This is a union of disjoint relations, where a runtime error is raised if the operands are not disjoint.

>>> :{
  rPrint$ s' `project` (undefined :: RHdr '[City])
          `dUnion`
          p' `project` (undefined :: RHdr '[City])
:}
┌─*** Exception: Arguments to dUnion are not disjoint, intersection:
┌────────┐
│ City   │
╞════════╡
│ London │
│ Paris  │
└────────┘
-}
dUnion ::
     (RDyadic ta1 ta a a1, Ord (HList ta), Typeable ta, Typeable a1,
      HFoldr (Mapcar HPresentTIP) [[String]] a1 [[String]]) =>
     Relation' ta -> Relation' ta1 -> Relation' ta
dUnion l r = 
    let u = Data.Set.union l $ relRearrange r
        in if size l + size r > size u
           then error $ "Arguments to dUnion are not disjoint, intersection:\n" ++ 
                        ( showTIPSetTab $ Data.Set.intersection l $ relRearrange r )
           else u

{-| Calculates the intersection of two relations.

Note how the name is different from Data.Set, where it is named "intersection". This is due to it being referred to as "intersect" in material describing the relational model; specifically named \"INTERSECT\" in Tutorial D.

Notably, for any given relation values r1 and r2 that are of the same type it holds that:

@r1 \`intersect\` r2 == r1 \`naturalJoin\` r2@

Intersection is therefore a special case of natural join within relational
theory.
-}
intersect :: (RDyadic ta l a a1, Ord (HList l)) =>
     Relation' l -> Relation' ta -> Relation' l
intersect l r = intersection l $ relRearrange r

{-| Calculates the difference of two relations.

The "minus" term is used in material describing relational theory; specifically Tutorial D names the operator \"MINUS\".
-}
minus :: (RDyadic ta l a a1, Ord (HList l)) =>
     Relation' l -> Relation' ta -> Relation' l
minus l r = difference l $ relRearrange r

-- | Exclusive union, aka. symmetric difference
xUnion :: (RDyadic arg ta arg' a1, Ord (HList ta),
      HProject (HList a1) (HList a1)) =>
     Set (TIP ta) -> Relation' arg -> Relation' ta
xUnion l r =
    let r1 = relRearrange r
        in ( Data.Set.union l r1 ) `minus` ( intersection l r1 )

-- | Alias of `xUnion`
xMinus
  :: (Eq (HList a1), Ord (HList ta), TagUntagFD arg' arg,
      TagUntagFD a1 ta, HAllTaggedEq ta, HAllTaggedEq arg,
      HLabelSet (LabelsOf arg), HLabelSet (LabelsOf ta),
      HProject (HList arg') (HList a1), HProject (HList a1) (HList a1),
      HAllTaggedLV ta, HAllTaggedLV arg) =>
     Set (TIP ta) -> Relation' arg -> Relation' ta
xMinus = xUnion
-- Or should that be sMinus?


extendByImage
  :: (Eq (HList l3), Ord t, Ord (HList l2), Ord (HList l1),
      HAllTaggedEq l2, HAllTaggedEq l1, HAllTaggedEq l3, HAllTaggedEq l,
      HLabelSet (LabelsOf l2), HLabelSet (LabelsOf l1),
      HLabelSet (LabelsOf l), HLabelSet (LabelsOf l3),
      HLabelSet (Label t ': LabelsOf l2),
      H2ProjectByLabels (LabelsOf l2) l4 l l1,
      H2ProjectByLabels (LabelsOf l3) l4 l3 b,
      H2ProjectByLabels (LabelsOf l4) l2 l3 b1, HAllTaggedLV l2,
      HAllTaggedLV l1, HAllTaggedLV l3, HAllTaggedLV l) =>
     Relation' l4
     -> Set (TIP l2)
     -> (Relation' l1 -> t)
     -> Set (TIP (Tagged t t ': l2))
extendByImage rel relP attOut = dExtend relP (\t -> attOut $ t `image` rel )


-- NOTE: The hlistOrRecord argument is redundant, at least for TIPs
{-| Groups the given relation on the given attributes into a given new attribute.

As the Tutorial D GROUP operator, not SQL GROUP BY.

>>> pt$ group sp' ( undefined :: RHdr '[PNO,QTY] ) PQ
┌────────────────────────────────────┬───────────────┐
│ PQ :: Relation '[PNO,QTY]          │ SNO :: String │
╞════════════════════════════════════╪═══════════════╡
│ ┌───────────────┬────────────────┐ │ S1            │
│ │ PNO :: String │ QTY :: Integer │ │               │
│ ╞═══════════════╪════════════════╡ │               │
│ │ P1            │ 300            │ │               │
│ │ P2            │ 200            │ │               │
...
└────────────────────────────────────┴───────────────┘
-}
group ::
     (Eq (HList l3), Ord t, Ord (HList l2), Ord (HList l1),
      HAllTaggedEq l2, HAllTaggedEq l1, HAllTaggedEq l3, HAllTaggedEq l,
      HAllTaggedEq l5, HLabelSet (LabelsOf l2), HLabelSet (LabelsOf l1),
      HLabelSet (LabelsOf l), HLabelSet (LabelsOf l3),
      HLabelSet (LabelsOf l5), HLabelSet (Label t ': LabelsOf l2),
      H2ProjectByLabels (LabelsOf l2) r l l1,
      H2ProjectByLabels (LabelsOf l3) r l3 b,
      H2ProjectByLabels (LabelsOf r) l2 l3 b1,
      H2ProjectByLabels (LabelsOf l4) r l5 l2, HAllTaggedLV l2,
      HAllTaggedLV l1, HAllTaggedLV l3, HAllTaggedLV l,
      HAllTaggedLV l5) =>
     Relation' r
     -> hlistOrRecord l4
     -> (Relation' l1 -> t)
     -> Set (TIP (Tagged t t ': l2))
group rel attsIn = extendByImage rel $ rel `projectAllBut` attsIn

infix 5 `group'`

{-| Alternative version of `group`. As with `rename'`:

>>> pt$ sp' `group'` ( (PNO,QTY) `as` PQ )

Same result as example for `group`.
-}
group' ::
     (Eq (HList l3), Ord t, Ord (HList l2), Ord (HList l1),
      HAllTaggedEq l2, HAllTaggedEq l1, HAllTaggedEq l3, HAllTaggedEq l,
      HAllTaggedEq l4, HLabelSet (LabelsOf l2), HLabelSet (LabelsOf l1),
      HLabelSet (LabelsOf l), HLabelSet (LabelsOf l3),
      HLabelSet (LabelsOf l4), HLabelSet (Label t ': LabelsOf l2),
      H2ProjectByLabels (LabelsOf l2) r l l1,
      H2ProjectByLabels (LabelsOf l3) r l3 b,
      H2ProjectByLabels (LabelsOf r) l2 l3 b1,
      H2ProjectByLabels (LabelsOf l5) r l4 l2, HAllTaggedLV l2,
      HAllTaggedLV l1, HAllTaggedLV l3, HAllTaggedLV l, HAllTaggedLV l4,
      RHdrClass a (hlistOrRecord l5)) =>
     Relation' r -> (a, Relation' l1 -> t) -> Set (TIP (Tagged t t ': l2))
group' rel (attsIn, attsOut) = extendByImage rel ( rel `projectAllBut'` attsIn ) attsOut

{-| Groups the given relation on all but the given attributes into a given new attribute.

>>> pt$ groupAllBut sp' ( undefined :: RHdr '[SNO] ) PQ

Same result as example for `group`.
-}
groupAllBut ::
     (Eq (HList l5), Ord e, Ord (HList l2), Ord (HList l3),
      HAllTaggedEq l2, HAllTaggedEq l3, HAllTaggedEq l5, HAllTaggedEq l,
      HRLabelSet l2, HRLabelSet l3, HRLabelSet l5, HRLabelSet l,
      HRLabelSet (Tagged e e ': l2),
      H2ProjectByLabels (LabelsOf l1) l4 l2 b,
      H2ProjectByLabels (LabelsOf l2) l4 l l3,
      H2ProjectByLabels (LabelsOf l5) l4 l5 b1,
      H2ProjectByLabels (LabelsOf l4) l2 l5 b2) =>
     Relation' l4
     -> hlistOrRecord l1
     -> (Relation' l3 -> e)
     -> Relation' (Tagged e e ': l2)
groupAllBut rel attsIn = extendByImage rel $ rel `project` attsIn


{-| Ungroups the given attribute of the given relation.

>>> sp' == ungroup ( group sp' ( undefined :: RHdr '[PNO,QTY] ) PQ ) ( undefined :: Label PQ )
True
-}
ungroup :: forall a a1 b s t t1 ta ta1 l1 r v .
     (Eq (HList a), Ord s, Ord (Unwrapped s), Ord (HList t),
      Ord (HList (HAppendListR t l1)), TagUntagFD a ta,
      TagUntagFD a1 ta1, HAllTaggedEq l1, HAllTaggedEq ta,
      HAllTaggedEq ta1, HAllTaggedEq t, HAllTaggedEq (HAppendListR t l1),
      HRLabelSet l1, HRLabelSet ta, HRLabelSet ta1, HRLabelSet t,
      HRLabelSet (HAppendListR t l1),
      H2ProjectByLabels (LabelsOf t) r ta l1,
      H2ProjectByLabels (LabelsOf r) t ta1 b,
      H2ProjectByLabels '[Label s] v t1 t, HProject (HList a1) (HList a),
      HOccurs s (TIP v), HAppendList t l1, Wrapped s,
      Unwrapped s ~ Relation' r) =>
     Relation' v -> Label s -> Relation' (HAppendListR t l1)
ungroup r a = fromList $ Data.Set.foldr f [] r
  where f = (\t b -> ( joinRight ( hDeleteAtLabel a t ) ( ( hOccurs t :: s ) ^. _Wrapped' ) ) ++ b )


{-| Tests whether the second argument is a proper subset of the first.

>>> relation [(SNO "S1", PNO "P4", QTY 200),(SNO "S2", PNO "P2", QTY 400)] `isProperSubsetOf` sp'
True
-}
isProperSubsetOf :: (RDyadic ta l a a1, Ord (HList l)) =>
  Relation' l -> Relation' ta -> Bool
isProperSubsetOf l r = Data.Set.isProperSubsetOf l ( relRearrange r )

{-| Tests whether the second argument is a subset of the first.

>>> relation [(SNO "S1", PNO "P4", QTY 200),(SNO "S2", PNO "P2", QTY 400)] `isSubsetOf` sp'
True
-}
isSubsetOf :: (RDyadic ta l a a1, Ord (HList l)) =>
  Relation' l -> Relation' ta -> Bool
isSubsetOf l r = Data.Set.isSubsetOf l ( relRearrange r )


{- Aggregation.

Aggregations are defined in terms of a fold. This works to produce either a simple
value:
*RelExample> :set -XViewPatterns
*RelExample> Data.Set.foldr (\ ( unTagTIP -> Foo fooa `HCons` ( Bar bara `HCons` ( Baz baza `HCons` HNil ) ) ) b -> fooa + b) 0 fbbRel
12
*RelExample> Data.Set.foldr (\ ( hOccurs -> ( Foo foo ) ) b -> foo + b) 0 fbbRel
12

Or an r-tuple:
*RelExample> pt$ fbbRel
┌───────────┬──────────────┬──────────────┐
│ Foo : Int │ Bar : String │ Baz : String │
╞═══════════╪══════════════╪══════════════╡
│ 5         │ "asdf"       │ "qwer"       │
│ 7         │ "werg"       │ "hotf"       │
└───────────┴──────────────┴──────────────┘
*RelExample> pt$ Data.Set.foldr (\ ( tipyTuple3 -> ( Foo fooa, Bar bara, Baz baza ) ) ( tipyTuple3 -> ( Foo foob, Bar barb, Baz bazb ) ) -> ( Foo ( fooa + foob ) .*. Bar ( bara ++ barb ) .*. Baz ( baza ++ bazb ) .*. emptyTIP )) (Foo 0 .*. Bar "" .*. Baz "" .*. emptyTIP) fbbRel
┌───────────┬──────────────┬──────────────┐
│ Foo : Int │ Bar : String │ Baz : String │
├───────────┼──────────────┼──────────────┤
│ 12        │ "asdfwerg"   │ "qwerhotf"   │
└───────────┴──────────────┴──────────────┘

As with extend and restrict this can be done in a more syntactically elegant manner:
-}

rfoldr1 :: (Foldable t, HOccurs e l) => (e -> b -> b) -> b -> t l -> b
rfoldr1 f b r = foldr (\t -> ( f ( hOccurs t ) ) ) b r
rfoldr2 f b r = foldr (\t -> ( f ( tipyTuple t ) ) ) b r
rfoldr3 f b r = foldr (\t -> ( f ( tipyTuple3 t ) ) ) b r
rfoldr4 f b r = foldr (\t -> ( f ( tipyTuple4 t ) ) ) b r
rfoldr5 f b r = foldr (\t -> ( f ( tipyTuple5 t ) ) ) b r

-- Variants where the accumulator/result is a tuple. Note that this does not have
-- to be the same tuple type as the body of the argument consists of, but there
-- does need to be the same number of elements in them.
{- Deprecated due to sillyness. TODO: Rework examples.
rfoldr1t f b r = foldr (\t1 t2 -> ( f ( hOccurs t1 ) ( hOccurs t2 ) ) ) b r
rfoldr2t f b r = foldr (\t1 t2 -> ( f ( tipyTuple t1 ) ( tipyTuple t2 ) ) ) b r
rfoldr3t f b r = foldr (\t1 t2 -> ( f ( tipyTuple3 t1 ) ( tipyTuple3 t2 ) ) ) b r
rfoldr4t f b r = foldr (\t1 t2 -> ( f ( tipyTuple4 t1 ) ( tipyTuple4 t2 ) ) ) b r
rfoldr5t f b r = foldr (\t1 t2 -> ( f ( tipyTuple5 t1 ) ( tipyTuple5 t2 ) ) ) b r
-}

{- Recalling fbbRel:
*RelExample> pt$ fbbRel
┌───────────┬──────────────┬──────────────┐
│ Foo : Int │ Bar : String │ Baz : String │
╞═══════════╪══════════════╪══════════════╡
│ 5         │ "asdf"       │ "qwer"       │
│ 7         │ "werg"       │ "hotf"       │
└───────────┴──────────────┴──────────────┘

*RelExample> rfoldr1 (\ ( Foo foo ) b -> foo + b) 0 fbbRel
12

*RelExample> pt$ rfoldr1t (\ ( Foo fooa ) ( Foo foob ) -> Foo ( fooa + foob ) .*. emptyTIP) (Foo 0 .*. emptyTIP) fbbRel
┌───────────┐
│ Foo : Int │
├───────────┤
│ 12        │
└───────────┘

*RelExample> pt$ rfoldr2t (\ ( Foo fooa, Bar bar ) ( Foo foob, Qux qux ) -> Foo ( fooa + foob ) .*. Qux ( bar ++ qux ) .*. emptyTIP) (Foo 0 .*. Qux "" .*. emptyTIP) fbbRel
┌───────────┬──────────────┐
│ Foo : Int │ Qux : String │
├───────────┼──────────────┤
│ 12        │ "asdfwerg"   │
└───────────┴──────────────┘

Due to the issue with the rfoldrNt arguments having to have same number of elements one will have to use the tipyTupleN functions manually with view-patterns to get something that isn't outrageously inelegant.

*RelExample> pt$ rfoldr3 (\ ( Foo fooa, Bar bar, Baz baz ) ( tipyTuple -> ( Foo foob, Qux qux ) ) -> Foo ( fooa + foob ) .*. Qux ( bar ++ "." ++ baz ++ ":" ++ qux ) .*. emptyTIP) (Foo 0 .*. Qux "" .*. emptyTIP) fbbRel
┌───────────┬────────────────────┐
│ Foo : Int │ Qux : String       │
├───────────┼────────────────────┤
│ 12        │ "asdfqwerwerghotf" │
└───────────┴────────────────────┘

Note the lack of a double underline in these results, which indicate that these
are reperesentations of r-tuples and not relations.
-}

{-| Right-fold of an attribute of a relation (although a "right" fold doesn't make sense in the context of the relational model). Note that the value of the third argument, `att`, is not used and may be "undefined".

>>> rafoldr (+) 0 ( Label :: Label QTY ) sp'
3100
>>> rafoldr (*) 1 ( Label :: Label QTY ) sp'
27648000000000000000000000000
-}
rafoldr :: (Foldable t, HasField l r s, Wrapped s) =>
     (Unwrapped s -> b -> b) -> b -> Label l -> t r -> b
rafoldr f b a r = foldr ( f . unwrapField a ) b r

{-| Concise variation of `rafoldr` that takes a data constructor instead of label.

>>> rafoldr' (+) 0 QTY sp'
3100
-}
rafoldr' :: forall t l r s b x. (Foldable t, HasField l r s, Wrapped s) =>
     (Unwrapped s -> b -> b) -> b -> ( x -> l ) -> t r -> b
rafoldr' f b _ r = foldr ( f . unwrapField ( Label :: Label l ) ) b r


{- | Attribute value aggregation, a specialization of `rafoldr` that aggregates of the values of a single attribute into a list of the values the attribute type wraps.

Note that the value of the first argument, `att`, is not used and may be "undefined".

>>> sum $ agg ( Label :: Label QTY ) sp'
3100
-}
agg :: (Foldable t, HasField l r s, Wrapped s) =>
     Label l -> t r -> [Unwrapped s]
agg a r = rafoldr (:) [] a r

{-| Concise variation of `agg` that takes a data constructor instead of label.

>>> sum $ agg' QTY sp'
3100
-}
agg' :: (Foldable t, HasField l r s, Wrapped s) =>
     (x -> l) -> t r -> [Unwrapped s]
agg' a r = rafoldr' (:) [] a r

{-| Right-fold of the attribute of an unary relation.

>>> rafoldrU (+) 0 $ sp' `project` ( undefined :: RHdr '[QTY] )
1000
>>> rafoldrU (*) 1 $ sp' `project` ( undefined :: RHdr '[QTY] )
2400000000
-}
rafoldrU :: (Foldable t, Wrapped y) =>
     (Unwrapped y -> b -> b) -> b -> t (TIP '[Tagged y y]) -> b
rafoldrU f b r = foldr ( f . unConsTagWrapU ) b r

{-| Aggregation of the single attribute of an unary relation. A specialization of `agg`, and thus in turn of `rafoldr`, that aggregates the single attribute of an unary relation, without requiring the name of that attribute.

>>> sum $ aggU $ sp' `project` ( undefined :: RHdr '[QTY] )
1000
-}
aggU :: (Foldable t, Wrapped y) =>
     t (TIP '[Tagged y y]) -> [Unwrapped y]
aggU r = rafoldrU (:) [] r


-- | Gives the cardinality of the argument.
count :: Set a -> Int
count = Data.Set.size

-- | Returns whether the given argument is empty or not.
isEmpty :: Set a -> Bool
isEmpty = Data.Set.null


{- Image Relations
Definition: Let relations r1 and r2 be joinable (i.e., such that attributes with the same name are of the same type); let t1 be a tuple of r1; let t2 be a tuple of r2 that has the same values for those common attributes as tuple t1 does; let relation r3 be that restriction of r2 that contains all and only such tuples t2; and let relation r4 be the projection of r3 on all but those common attributes. Then r4 is the image relation (with respect to r2) corresponding to t1.
 - Chris J. Date, SQL and Relational Theory 2nd ed. p 136
-}

-- Project tuple on relation heading
projectTOnR :: forall l l1 t b .
     (HAllTaggedEq l, HRLabelSet l, H2ProjectByLabels (LabelsOf l1) t l b) =>
     TIP t -> Relation' l1 -> TIP l
projectTOnR t _ = tipyProject ( undefined :: Proxy ( LabelsOf l1 ) ) t

-- Specialization of restriction that takes a relation and a tuple whose heading
-- is a (not neccesarily proper) subset of the heading of the relation, and
-- restricts the relation by the intersection of attributes of the relation and
-- tuple.
restrictByTuple ::
     (Eq (HList l), HAllTaggedEq l, HRLabelSet l,
      H2ProjectByLabels (LabelsOf l) t l b) =>
     Relation' t -> TIP l -> Relation' t
restrictByTuple r t = Data.Set.filter (\a -> tipyProject ( labelsOf t ) a == t ) r
-- TODO: It would possibly be more performant and still correct to use partition
-- instead of filter, and feed the remainder part of the result of that back into
-- this function as parameter r.


{-| Calculates the image of a relation corresponding to an r-tuple.

An application of the first argument only, an r-tuple, to this function yields what is known as the @!!@ operator in Tutorial D.

>>> rPrint$ rTuple (SNO "S2", PNO "PXX")
┌─────┬─────┐
│ SNO │ PNO │
├─────┼─────┤
│ S2  │ PXX │
└─────┴─────┘
>>> rPrint$ rTuple (SNO "S2", PName "Nut")
┌─────┬───────┐
│ SNO │ PName │
├─────┼───────┤
│ S2  │ Nut   │
└─────┴───────┘
>>> rPrint$ rTuple (SNO "S2", PNO "P1") `image` sp'
┌─────┐
│ QTY │
╞═════╡
│ 300 │
└─────┘
>>> rPrint$ rTuple (SNO "S2", PName "Nut") `image` sp'
┌─────┬─────┐
│ PNO │ QTY │
╞═════╪═════╡
│ P1  │ 300 │
│ P2  │ 400 │
└─────┴─────┘
-}
image ::
     (Eq (HList l3), Ord (HList l1), HAllTaggedEq l1, HAllTaggedEq l,
      HAllTaggedEq l3, HRLabelSet l1, HRLabelSet l, HRLabelSet l3,
      H2ProjectByLabels (LabelsOf l2) l4 l l1,
      H2ProjectByLabels (LabelsOf l3) l4 l3 b,
      H2ProjectByLabels (LabelsOf l4) l2 l3 b1) =>
     TIP l2 -> Relation' l4 -> Relation' l1
image t r = projectAllBut ( restrictByTuple r $ projectTOnR t r ) t

-- Extend with image relation.
-- Deprecated, TODO: Replace examples with plain extend and cp[n]
{-
extend' ::
     (Eq (HList l3), Ord e, Ord (HList l1), Ord (HList ta),
      TagUntagFD a ta, HAllTaggedEq l1, HAllTaggedEq l3, HAllTaggedEq l,
      HAllTaggedEq ta, HRLabelSet l1, HRLabelSet l3, HRLabelSet l,
      HRLabelSet ta, HRLabelSet (Tagged e e ': ta),
      H2ProjectByLabels (LabelsOf ta) l4 l l1,
      H2ProjectByLabels (LabelsOf l3) l4 l3 b,
      H2ProjectByLabels (LabelsOf l4) ta l3 b1) =>
     Relation' ta -> ((Relation' l4 -> Relation' l1) -> HList a -> e)
     -> Relation' (Tagged e e ': ta)
extend' r f = Data.Set.map (\t -> ( f ( image t ) ( unTagTIP t ) ) .*. t ) r
-}

{-
*RelExample> pt$ ( fbbRel `union` fbbRel2 ) `extend'` (\ imgRel ( Foo foo `HCons` ( Bar bar `HCons` ( Baz baz `HCons` emptyTIP ) ) ) -> RFrotz $ imgRel ffRel2 )
┌────────────────────────────┬───────────┬──────────────┬──────────────┐
│ RFrotz : Relation '[Frotz] │ Foo : Int │ Bar : String │ Baz : String │
╞════════════════════════════╪═══════════╪══════════════╪══════════════╡
| ┌────────────────┐         │ 12        │ "asdf"       │ "qwer"       |
| │ Frotz : String │         │           │              │              |
| ╞════════════════╡         │           │              │              |
| └────────────────┘         │           │              │              |
| ┌────────────────┐         │ 5         │ "asdf"       │ "qwer"       |
| │ Frotz : String │         │           │              │              |
| ╞════════════════╡         │           │              │              |
| | "Bork!"        |         │           │              │              |
| | "Burk!"        |         │           │              │              |
| └────────────────┘         │           │              │              |
| ┌────────────────┐         │ 7         │ "werg"       │ "hotf"       |
| │ Frotz : String │         │           │              │              |
| ╞════════════════╡         │           │              │              |
| | "ljkh"         |         │           │              │              |
| └────────────────┘         │           │              │              |
└────────────────────────────┴───────────┴──────────────┴──────────────┘
Cue Thus Spake Zarathustra.


-- TODO: Is it possible to make this work without a type for imgRel?
*RelExample> pt$ ( fbbRel `union` fbbRel2 ) `extend'` (\ ( _ ) ( Foo foo `HCons` ( Bar bar `HCons` ( Baz baz `HCons` emptyTIP ) ) )  -> Qux $ ( bar ++ "," ++ baz ) )
 * BOOM *

One idea is defining a class with 'extend' as a function, with the empty tuple for one instance.

-- TODO2: Or with different image targets, for that matter? Something along these
lines must also be made to work for this to be in accordance with how image
relations work in relational theory:
*RelExample> pt$ ( fbbRel `union` fbbRel2 ) `extend'` (\ ( imgRel ) ( Foo foo `HCons` ( Bar bar `HCons` ( Baz baz `HCons` emptyTIP ) ) )  -> Qux $ ( bar ++ "," ++ baz ++ "; " ++ ( show $ imgRel ffRel' ) ++ "; " ++ ( show $ imgRel fzRel ) ) )
<interactive>:515:218:
    Couldn't match type ‘Qux’ with ‘Frotz’
    Expected type: Set (TIP '[Tagged Foo Foo, Tagged Frotz Frotz])
      Actual type: Set
                     (TIP '[Tagged Foo Foo, Tagged Qux Qux, Tagged Zup Zup])
    In the first argument of ‘imgRel’, namely ‘fzRel’
    In the second argument of ‘($)’, namely ‘imgRel fzRel’
-}
{- More examples (TODO: Work into RelExample.hs):

A binary RVA:
*RelExample> pt$ ( fbbRel `union` fbbRel2 ) `extend'` (\ imgRel _ -> RQuxZup $ ( imgRel fqzRel ) )
┌──────────────────────────────────────────────────────────────┬───────────┬──────────────┬──────────────┐
│ RQuxZup : Relation '[Qux,Zup]                                │ Foo : Int │ Bar : String │ Baz : String │
╞══════════════════════════════════════════════════════════════╪═══════════╪══════════════╪══════════════╡
│ fromList []                                                  │ 12        │ "asdf"       │ "qwer"       │
│ fromList [TIPH[Qux "Bork!",Zup 99],TIPH[Qux "Burk!",Zup 98]] │ 5         │ "asdf"       │ "qwer"       │
│ fromList [TIPH[Qux "ljkh",Zup 97]]                           │ 7         │ "werg"       │ "hotf"       │
└──────────────────────────────────────────────────────────────┴───────────┴──────────────┴──────────────┘

Extension together with image relations also form a generalization of summarization (SQL 'aggregation'):
*RelExample> pt$ ( fbbRel `union` fbbRel2 ) `extend'` (\ imgRel _ -> RZup $ ( imgRel fzRel ) )
┌──────────────────────────────────────┬───────────┬──────────────┬──────────────┐
│ RZup : Relation '[Zup]               │ Foo : Int │ Bar : String │ Baz : String │
╞══════════════════════════════════════╪═══════════╪══════════════╪══════════════╡
│ fromList []                          │ 12        │ "asdf"       │ "qwer"       │
│ fromList [TIPH[Zup 97]]              │ 7         │ "werg"       │ "hotf"       │
│ fromList [TIPH[Zup 98],TIPH[Zup 99]] │ 5         │ "asdf"       │ "qwer"       │
└──────────────────────────────────────┴───────────┴──────────────┴──────────────┘
*RelExample> pt$ ( fbbRel `union` fbbRel2 ) `extend'` (\ imgRel _ -> Zaf $ sum $ agg ( undefined :: Zup ) $ imgRel fzRel )
┌───────────┬───────────┬──────────────┬──────────────┐
│ Zaf : Int │ Foo : Int │ Bar : String │ Baz : String │
╞═══════════╪═══════════╪══════════════╪══════════════╡
│ 0         │ 12        │ "asdf"       │ "qwer"       │
│ 97        │ 7         │ "werg"       │ "hotf"       │
│ 197       │ 5         │ "asdf"       │ "qwer"       │
└───────────┴───────────┴──────────────┴──────────────┘
TODO: Aggregation of multiple attributes.

And of course, pipe this in your stuff and smoke it:
*RelExample> pt$ fbbRel3
┌───────────┬──────────────┬──────────────┐
│ Foo : Int │ Bar : String │ Baz : String │
╞═══════════╪══════════════╪══════════════╡
│ 5         │ "asdf"       │ "qwer"       │
│ 5         │ "huof"       │ "90ujv"      │
│ 5         │ "iouh"       │ "89y3t"      │
│ 42        │ "bhidf"      │ "y98gf"      │
│ 42        │ "q98hf"      │ "zcxv"       │
└───────────┴──────────────┴──────────────┘
*RelExample> pt$ ( project fbbRel3 ( undefined :: RHdr '[Foo] ) )
┌───────────┐
│ Foo : Int │
╞═══════════╡
│ 5         │
│ 42        │
└───────────┘
*RelExample> pt$ ( project fbbRel3 ( undefined :: RHdr '[Foo] ) ) `extend` (\ ( image -> imgRel ) -> RBarBaz $ imgRel fbbRel3 )
┌─────────────────────────────────┬───────────┐
│ RBarBaz : Relation '[Bar,Baz]   │ Foo : Int │
╞═════════════════════════════════╪═══════════╡
| ┌──────────────┬──────────────┐ │ 5         |
| │ Bar : String │ Baz : String │ │           |
| ╞══════════════╪══════════════╡ │           |
| | "asdf"       │ "qwer"       | │           |
| | "huof"       │ "90ujv"      | │           |
| | "iouh"       │ "89y3t"      | │           |
| └──────────────┴──────────────┘ │           |
| ┌──────────────┬──────────────┐ │ 42        |
| │ Bar : String │ Baz : String │ │           |
| ╞══════════════╪══════════════╡ │           |
| | "bhidf"      │ "y98gf"      | │           |
| | "q98hf"      │ "zcxv"       | │           |
| └──────────────┴──────────────┘ │           |
└─────────────────────────────────┴───────────┘
-}

