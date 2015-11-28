{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Algebra
Description : Relational algebra, including supporting functions
Copyright   : © Thor Michael Støre, 2015
License     : GPL v2 without "any later version" clause
Maintainer  : thormichael át gmail døt com
Stability   : experimental

Relational algebra based on HList records.

It is important to note that, in order to build a straight forward foundation,
this module defines pure functions, viz. they only operate upon relational
/values/, not relvars or the result of expressions involving relvars. See
"Database.HaskRel.Relational.Expression" for functions that function as conveyed
by the relational model, which are the ones that are intended to be used
directly.

All the examples in "Database.HaskRel.Relational.Expression" are defined in
terms of the relvars s, p and sp; to run the examples in that module with the
functions of this module one can use the relation values s', p' and sp'
instead. The script examples\/algebraExample.sh starts a GHCi session with the
imports and pragmas required to run these examples.
-}
module Database.HaskRel.Relational.Algebra (
  -- * Operators of the relational algebra
  rename, extend, restrict, project, projectAllBut,
  naturalJoin, nJoin, times, matching, semiJoin, notMatching, semiDiff,
  union, dUnion, intersect, minus, xUnion, group, groupAllBut, ungroup,
  -- * Somewhat deprecated operators of the relational algebra
  summarize,
  -- * Additional operators with relational closure
  interJoin, iJoin, dExtend, aSummarize,
  imageExtendL, minus_, tClose,
  -- ** Alternative, concise functions
  extendA, dExtendA, renameA,
  -- * Supporting functions
  isProperSubsetOf, isSubsetOf, image, count, isEmpty,
  -- * Other useful, non-relational associated functions
  rafoldr, rafoldrU, agg, aggU, relRearrange,
  -- * Instances
  NotEmpty, Relabel
   ) where

-- TODO: See also Database.HaskRel.Relational.TIP.Algebra for TODOs.

import Data.Set ( Set, fromList, toList, size, intersection, difference )
import qualified Data.Set

import Database.HaskRel.HFWTabulation ( showHRecSetTab, HPresentRecAttr )
import Database.HaskRel.Relational.Definition ( Relation, unordRecEq, relRearrange, as )

import Data.HList.CommonMain

import Data.Typeable



data Relabel tr = Relabel tr

instance (HasFieldM l r v, Label dl ~ (DemoteMaybe (Label l) v),
          b ~ Tagged dl a) =>
     ApplyAB (Relabel r) (Tagged l a) b
  where
     applyAB _ (Tagged a) = Tagged a

{-| Rename of multiple labels, preserving original order.

>>> let aRecord = ((Tagged 1 :: Tagged "g" Int) .*. (Tagged 2 :: Tagged "a" Int) .*. (Tagged 3 :: Tagged "i" Int) .*. (Tagged 4 :: Tagged "c" Int) .*. (Tagged 5 :: Tagged "e" Int) .*. emptyRecord)
>>> aRecord
Record{g=1,a=2,i=3,c=4,e=5}
>>> rhRenameLabels ((Label::Label "c") .=. (Label::Label "d") .*. (Label::Label "a") .=. (Label::Label "b") .*. emptyRecord) aRecord
Record{g=1,b=2,i=3,d=4,e=5}
-}
rhRenameLabels
  :: (HLabelSet (LabelsOf r), HMapAux HList (Relabel tr) a r,
      SameLength' r a, SameLength' a r, HAllTaggedLV r) =>
     tr -> Record a -> Record r
rhRenameLabels a (Record r) = mkRecord $ hMap (Relabel a) r

{-
-- | Alternative implementation of 'rhRenameLabels', building on 'relabeled'.
rhRenameLabels'
  :: (RecordValues a, RecordValues a1, HLabelSet (LabelsOf a),
      HMapAux HList TaggedFn (RecordValuesR a) a1,
      HMapAux HList TaggedFn (RecordValuesR a1) a,
      HMapAux HList (Relabel tr) a1 a, SameLength' a a1,
      SameLength' a (RecordValuesR a1), SameLength' a1 a,
      SameLength' a1 (RecordValuesR a), SameLength' (RecordValuesR a) a1,
      SameLength' (RecordValuesR a1) a, HAllTaggedLV a,
      RecordValuesR a1 ~ RecordValuesR a) =>
     tr -> Record a1 -> Record a
rhRenameLabels' _ r = r ^. relabeled
-}

{-| Rename multiple attributes of a relation.

>>> let pnu = Label :: Label "pnu"
>>> let colour = Label :: Label "colour"
>>> rPrint$ p' `rename` nAs( pno `as` pnu, color `as` colour )
┌─────┬───────┬────────┬────────┬────────┐
│ pnu │ pName │ colour │ weight │ city   │
╞═════╪═══════╪════════╪════════╪════════╡
│ P1  │ Nut   │ Red    │ 12 % 1 │ London │
...
-}
rename
  :: (Ord (HList r), HLabelSet (LabelsOf r),
      HMapAux HList (Relabel tr) a r, SameLength' r a, SameLength' a r,
      HAllTaggedLV r) =>
     Relation a -> tr -> Relation r
rename r l = Data.Set.map (rhRenameLabels l) r

{- | Renames a single attribute. See
'Database.HaskRel.Relational.Expression.renameA'
-}
renameA
  :: forall l1 v1 r v' v l . (Ord (HExtendR (Tagged l1 v1) (r v')), HasField l (r v) v1,
      HExtend (Tagged l1 v1) (r v'), HDeleteAtLabel r l v v') =>
     Set (r v) -> Tagged l (Label l1) -> Set (HExtendR (Tagged l1 v1) (r v'))
renameA r frto = Data.Set.map (hRenameLabel (Label::Label l) (untag frto)) r


{- | Extends the given relation with the r-tuple resulting from the second
argument. Existing attributes with the same name will be replaced.

See 'Database.HaskRel.Relational.Expression.extend'.
-}
extend
  :: (Ord (HList (HAppendListR r r'1)),
      HLabelSet (LabelsOf (HAppendListR r r'1)),
      HDeleteLabels (LabelsOf r) r' r'1, HAppendList r r'1,
      HAllTaggedLV (HAppendListR r r'1)) =>
     Relation r' -> (Record r' -> Record r) -> Relation (HAppendListR r r'1)
extend r f = Data.Set.map (\t -> f t .<++. t ) r

{- | Extends the given relation with the attribute resulting from the second
argument. If an attribute with the same name exists then it will be
replaced. This allows for the function of the second argument to be simpler.

See 'Database.HaskRel.Relational.Expression.extendA'.
-}
extendA :: forall r l e v v'.
     (Ord (HExtendR (Tagged l e) (r v')), HExtend (Tagged l e) (r v'),
      HDeleteAtLabel r l v v') =>
     Set (r v) -> (r v -> Tagged l e) -> Set (HExtendR (Tagged l e) (r v'))
extendA r f = Data.Set.map (\t -> f t .*. hDeleteAtLabel (Label::Label l) t) r

-- TODO: An attempt to define a class with instances for what is now extend and
-- extendA resulted in something that broke inference. Try again? Is it also
-- possible make extendA a class, with an instance that uses .*. without
-- hDeleteByLabel, and one that uses .@.? I can't see a way to. Probably better
-- to make a quasiquoter, as mentioned in Expression.hs.


{- | Disjoint extension. Extends the given relation with the result of the second
argument, as 'extend', but without deleting any that exist.

See 'Database.HaskRel.Relational.Expression.dExtend'.
-}
dExtend
  :: (Ord (HList (HAppendListR r r'1)),
      HLabelSet (LabelsOf (HAppendListR r r'1)),
      HDeleteLabels (LabelsOf r) r' r'1, HAppendList r r'1,
      HAllTaggedLV (HAppendListR r r'1),
      HRLabelSet (HAppendListR r' r) -- Added to enforce disjoint structures
      ) =>
     Relation r' -> (Record r' -> Record r) -> Relation (HAppendListR r r'1)
dExtend = extend

{- | Disjoint extension of a single attribute. Extends the given relation with the
result of the second argument, as 'extend', but without deleting any that
exist. @l@ cannot already have any attribute @e@.

See 'Database.HaskRel.Relational.Expression.dExtendA'.
-}
dExtendA :: (Ord (HExtendR e l), HExtend e l) =>
     Set l -> (l -> e) -> Set (HExtendR e l)
dExtendA r f = Data.Set.map (\t -> f t .*. t) r


-- TODO: imageExtend: Use type level concatenation to build a label from the
-- labels of the body
{-
type family (++) (as :: [k]) (bs :: [k]) :: [k] where
  (++) a '[] = a
  (++) '[] b = b
  (++) (a ': as) bs = a ': (as ++ bs)
-}

{- | Extends the first given relation with an attribute resulting from imaging
each tuple of said relation against the second given relation. This gives a
superset of the information given by SQL @RIGHT OUTER JOIN@.

See 'Database.HaskRel.Relational.Expression.imageExtendL'.
-}
imageExtendL
  :: (Eq (HList l), Ord (HList l1), Ord (HList r'),
      HLabelSet (LabelsOf l), HLabelSet (Label t ': LabelsOf l1),
      HDeleteLabels (LabelsOf l1) r r',
      H2ProjectByLabels (LabelsOf l) r l b,
      H2ProjectByLabels (LabelsOf r) l1 l b1, HAllTaggedLV l1,
      HAllTaggedLV l) =>
     Set (Record l1)
     -> Relation r
     -> Label t
     -> Set (Record (Tagged t (Relation r') ': l1))
imageExtendL r1 r2 l = dExtendA r1 (\t -> l .=. t `image` r2)


{- | Restricts the given relation according to the given predicate. Note that this
is the well known 'WHERE' operator of both SQL and Tutorial D, but since "where"
is a reserved keyword in Haskell it is named "restrict".

See 'Database.HaskRel.Relational.Expression.restrict'.
-}
restrict :: Set a -> (a -> Bool) -> Set a
restrict r f = Data.Set.filter f r


-- TODO: Fail if any element of the second argument is not part of the first.

{- | Projects the given relation on the given heading.

See 'Database.HaskRel.Relational.Expression.project'.
-}
project
  :: (Ord (HList a), HLabelSet (LabelsOf a),
      H2ProjectByLabels ls t a b, HAllTaggedLV a) =>
     Relation t -> proxy ls -> Relation a
project r h = Data.Set.map ( hProjectByLabels h ) r

{- | Projects the given relation on the heading of said given relation minus the
given heading.

See 'Database.HaskRel.Relational.Expression.projectAllBut'.
-}
projectAllBut
  :: (Ord (HList r'), HDeleteLabels ks r r') =>
     Relation r -> proxy ks -> Relation r'
projectAllBut r a = Data.Set.map ( hDeleteLabels a ) r


{- | Performs a union of the given relations.

See 'Database.HaskRel.Relational.Expression.union'.
-}
union
  :: (Ord (HList l), HRearrange3 (LabelsOf l) r l,
      HLabelSet (LabelsOf l), SameLength' r l,
      SameLength' r (LabelsOf l), SameLength' l r,
      SameLength' (LabelsOf l) r) =>
     Relation l -> Relation r -> Relation l
union r1 r2 = Data.Set.union r1 ( relRearrange r2 )


-- TODO: This should not just cause "error". Perhaps an Either String (Relation
-- a) or Either (Relation ?) (Relation a) instead of causing an error. Will have
-- to amend at least HFWPresent if so.

{- | Performs a disjoint union between the two relvars. This is a union of
disjoint relations, where a runtime error is raised if the operands are not
disjoint.

See 'Database.HaskRel.Relational.Expression.dUnion'.
-}
dUnion
  :: (Ord (HList a), Typeable a, RecordValues a,
      HRearrange3 (LabelsOf a) r a, HLabelSet (LabelsOf a),
      HFoldr (Mapcar HPresentRecAttr) [[String]] (RecordValuesR a) [[String]],
      SameLength' a r, SameLength' r a, SameLength' r (LabelsOf a),
      SameLength' (LabelsOf a) r) =>
     Relation a -> Relation r -> Relation a
dUnion l r =
  let u = Data.Set.union l $ relRearrange r
   in if size l + size r > size u
        then error $ "Arguments to dUnion are not disjoint, intersection:\n" ++ 
                     showHRecSetTab ( Data.Set.intersection l $ relRearrange r )
        else u


{-| The intersection of two relations.

Note how the name is different from Data.Set, where the comparable function is
named "intersection". This is due to it being referred to as "intersect" in
material describing the relational model; specifically named \"INTERSECT\" in
Tutorial D.

See 'Database.HaskRel.Relational.Expression.intersect'.
-}
intersect
  :: (Ord (HList l), HRearrange3 (LabelsOf l) r l,
      HLabelSet (LabelsOf l), SameLength' r l,
      SameLength' r (LabelsOf l), SameLength' l r,
      SameLength' (LabelsOf l) r) =>
     Relation l -> Relation r -> Relation l
intersect l r = intersection l $ relRearrange r

{-| The difference of two relations.

The "minus" term is used in material describing relational theory; specifically
Tutorial D names the operator \"MINUS\".

See 'Database.HaskRel.Relational.Expression.minus'.
-}
minus
  :: (Ord (HList l), HRearrange3 (LabelsOf l) r l,
      HLabelSet (LabelsOf l), SameLength' r l,
      SameLength' r (LabelsOf l), SameLength' l r,
      SameLength' (LabelsOf l) r) =>
     Relation l -> Relation r -> Relation l
minus l r = difference l $ relRearrange r

{-| The difference of two relations. This differs from 'minus' in that the
attribute order of the second argument takes precedence, which is neccessary to
swap precedence since 'minus' is non-commutative. This function is as such equal
to 'minus' as far as relational theory is concerned, the difference is on a
lower level of abstraction.
-}
minus_
  :: (Ord (HList l), HRearrange3 (LabelsOf l) r l,
      HLabelSet (LabelsOf l), SameLength' r l,
      SameLength' r (LabelsOf l), SameLength' l r,
      SameLength' (LabelsOf l) r) =>
     Relation r -> Relation l -> Relation l
minus_ = difference . relRearrange
-- TODO: Swapped versions of all r-dyadic, commutative functions.


{- | Exclusive union, aka. symmetric difference.

See 'Database.HaskRel.Relational.Expression.xUnion'.
-}
xUnion
  :: (Ord (HList r1), HRearrange3 (LabelsOf r1) r r1,
      HRearrange3 (LabelsOf r1) r1 r1, HLabelSet (LabelsOf r1),
      SameLength' r r1, SameLength' r (LabelsOf r1), SameLength' r1 r,
      SameLength' r1 r1, SameLength' r1 (LabelsOf r1),
      SameLength' (LabelsOf r1) r, SameLength' (LabelsOf r1) r1) =>
     Relation r1 -> Relation r -> Relation r1
xUnion l r =
    let r1 = relRearrange r
     in Data.Set.union l r1 `minus` intersection l r1


{- | Performs a natural join of the two given relations.

See 'Database.HaskRel.Relational.Expression.naturalJoin'.
-}
naturalJoin
  :: (Eq (HList l), Ord (HList (HAppendListR t1 t2)),
      HRearrange3 (LabelsOf l) r l, HLabelSet (LabelsOf l),
      HLabelSet (LabelsOf t2), HLabelSet (LabelsOf r),
      HLabelSet (LabelsOf (HAppendListR t1 t2)),
      H2ProjectByLabels (LabelsOf t1) t l t2,
      H2ProjectByLabels (LabelsOf t) t1 r b, HAppendList t1 t2,
      SameLength' l r, SameLength' r l, SameLength' r (LabelsOf l),
      SameLength' (LabelsOf l) r, HAllTaggedLV t2, HAllTaggedLV l,
      HAllTaggedLV r, HAllTaggedLV (HAppendListR t1 t2)) =>
     Relation t1
     -> Relation t -> Relation (HAppendListR t1 t2)
naturalJoin r1 r2 = fromList $ Data.Set.foldr (\t1 b -> joinTOnR t1 r2 ++ b ) [] r1

{- | Alias of 'naturalJoin'.

See 'Database.HaskRel.Relational.Expression.nJoin'.
-}
nJoin
  :: (Eq (HList l), Ord (HList (HAppendListR t1 t2)),
      HRearrange3 (LabelsOf l) r l, HLabelSet (LabelsOf l),
      HLabelSet (LabelsOf t2), HLabelSet (LabelsOf r),
      HLabelSet (LabelsOf (HAppendListR t1 t2)),
      H2ProjectByLabels (LabelsOf t1) t l t2,
      H2ProjectByLabels (LabelsOf t) t1 r b, HAppendList t1 t2,
      SameLength' l r, SameLength' r l, SameLength' r (LabelsOf l),
      SameLength' (LabelsOf l) r, HAllTaggedLV t2, HAllTaggedLV l,
      HAllTaggedLV r, HAllTaggedLV (HAppendListR t1 t2)) =>
     Relation t1 -> Relation t -> Relation (HAppendListR t1 t2)
nJoin = naturalJoin

joinTOnR
  :: (Eq (HList l), HRearrange3 (LabelsOf l) r l,
      HLabelSet (LabelsOf l), HLabelSet (LabelsOf t2),
      HLabelSet (LabelsOf r), HLabelSet (LabelsOf (HAppendListR t1 t2)),
      H2ProjectByLabels (LabelsOf t1) t l t2,
      H2ProjectByLabels (LabelsOf t) t1 r b, HAppendList t1 t2,
      SameLength' l r, SameLength' r l, SameLength' r (LabelsOf l),
      SameLength' (LabelsOf l) r, HAllTaggedLV t2, HAllTaggedLV l,
      HAllTaggedLV r, HAllTaggedLV (HAppendListR t1 t2)) =>
     Record t1 -> Relation t -> [Record (HAppendListR t1 t2)]
joinTOnR t1 r2 = Data.Set.foldr (tupJoin t1) [] r2
  where
    tupJoin t1' t2' b =
      let
        (p',cP) = hProjectByLabels2 ( labelsOf t1' ) t2'
      in
        if p' `unordRecEq` hProjectByLabels ( labelsOf t2' ) t1'
        then hAppend t1' cP : b
        else b

{- | The cartesian product of two relations. A specialized natural join; the
natural join between two relations with disjoint headings.

See 'Database.HaskRel.Relational.Expression.times'.
-}
times
  :: (HRLabelSet (HAppendListR t r), -- This is added, to ensure that t and t1 are disjoint.
      Eq (HList l), Ord (HList (HAppendListR t1 t2)),
      HRearrange3 (LabelsOf l) r l, HLabelSet (LabelsOf l),
      HLabelSet (LabelsOf t2), HLabelSet (LabelsOf r),
      HLabelSet (LabelsOf (HAppendListR t1 t2)),
      H2ProjectByLabels (LabelsOf t1) t l t2,
      H2ProjectByLabels (LabelsOf t) t1 r b, HAppendList t1 t2,
      SameLength' l r, SameLength' r l, SameLength' r (LabelsOf l),
      SameLength' (LabelsOf l) r, HAllTaggedLV t2, HAllTaggedLV l,
      HAllTaggedLV r, HAllTaggedLV (HAppendListR t1 t2)) =>
     Relation t1
     -> Relation t -> Relation (HAppendListR t1 t2)
times = naturalJoin


-- TODO: Get this working, now it just ends up with overlapping error instead of
-- the Fail IsEmpty
data IsEmpty

-- | Failure class restricting a type-level operation to a non-empty result.
class NotEmpty ( l :: [*] )
instance NotEmpty l -- Is there any way to match on "anything that is not '[]"?
instance ( Fail IsEmpty ) => NotEmpty '[]

-- Finding out that when : is induced one must simply use ': instead was
-- frustrating

{-| Performs a natural join between two relations with intersecting headings. A
specialized natural join.

A join upon relations r1, r2 where the intersection of the heading of r1 and of
r2 is not empty; the headings are not disjoint. This is the complement of
'times' that together with it forms a natural join; all that would be disallowed
for @times@ is allowed here and vice-versa. The name is what I quickly settled
on, suggestions for a better one would be welcome. (Attribute-Intersecting
Natural Join is another candidate.)

This function doesn't have a specific identity value, although it holds that
@r \`interJoin\` r = r@

See 'Database.HaskRel.Relational.Expression.interJoin'.
-}
interJoin
  :: (HTIntersect (LabelsOf r) (LabelsOf t) i, NotEmpty i, -- i must not be '[]
      Eq (HList l), Ord (HList (HAppendListR t1 t2)),
      HRearrange3 (LabelsOf l) r l, HLabelSet (LabelsOf l),
      HLabelSet (LabelsOf t2), HLabelSet (LabelsOf r),
      HLabelSet (LabelsOf (HAppendListR t1 t2)),
      H2ProjectByLabels (LabelsOf t1) t l t2,
      H2ProjectByLabels (LabelsOf t) t1 r b, HAppendList t1 t2,
      SameLength' l r, SameLength' r l, SameLength' r (LabelsOf l),
      SameLength' (LabelsOf l) r, HAllTaggedLV t2, HAllTaggedLV l,
      HAllTaggedLV r, HAllTaggedLV (HAppendListR t1 t2)) =>
     Relation t1 -> Relation t -> Relation (HAppendListR t1 t2)
interJoin = naturalJoin

-- | Alias of 'interJoin'. See 'Database.HaskRel.Relational.Expression.iJoin'.
iJoin
  :: (Eq (HList l), Ord (HList (HAppendListR t1 t2)),
      HRearrange3 (LabelsOf l) r l, HLabelSet (LabelsOf l),
      HLabelSet (LabelsOf r), HLabelSet (LabelsOf t2),
      HLabelSet (LabelsOf (HAppendListR t1 t2)),
      H2ProjectByLabels (LabelsOf t) t1 r b,
      H2ProjectByLabels (LabelsOf t1) t l t2,
      HTIntersect (LabelsOf r) (LabelsOf t) i, HAppendList t1 t2,
      SameLength' r l, SameLength' r (LabelsOf l), SameLength' l r,
      SameLength' (LabelsOf l) r, HAllTaggedLV t2, HAllTaggedLV r,
      HAllTaggedLV l, HAllTaggedLV (HAppendListR t1 t2), NotEmpty i) =>
     Relation t1 -> Relation t -> Relation (HAppendListR t1 t2)
iJoin = interJoin


{- | Performs a semi-join of the first given relation against the second given
relation.

See 'Database.HaskRel.Relational.Expression.matching'.
-}
matching
  :: (Eq (HList l), Ord (HList t), HRearrange3 (LabelsOf l) r l,
      HLabelSet (LabelsOf l), HLabelSet (LabelsOf r),
      H2ProjectByLabels (LabelsOf t) l1 l b,
      H2ProjectByLabels (LabelsOf l1) t r b1, SameLength' l r,
      SameLength' r l, SameLength' r (LabelsOf l),
      SameLength' (LabelsOf l) r, HAllTaggedLV l, HAllTaggedLV r) =>
     Relation t -> Relation l1 -> Relation t
matching = semiJoin

-- | Alias of 'matching'. See 'Database.HaskRel.Relational.Expression.semiJoin'.
semiJoin
  :: (Eq (HList l), Ord (HList t), HRearrange3 (LabelsOf l) r l,
      HLabelSet (LabelsOf l), HLabelSet (LabelsOf r),
      H2ProjectByLabels (LabelsOf t) l1 l b,
      H2ProjectByLabels (LabelsOf l1) t r b1, SameLength' l r,
      SameLength' r l, SameLength' r (LabelsOf l),
      SameLength' (LabelsOf l) r, HAllTaggedLV l, HAllTaggedLV r) =>
     Relation t -> Relation l1 -> Relation t
semiJoin r1 r2 = fromList $ Data.Set.foldr (\t1 b -> semiJoinTOnR t1 ( toList r2 ) ++ b) [] r1

semiJoinTOnR
  :: (Eq (HList l), HRearrange3 (LabelsOf l) r l,
      HLabelSet (LabelsOf l), HLabelSet (LabelsOf r),
      H2ProjectByLabels (LabelsOf t) l1 l b,
      H2ProjectByLabels (LabelsOf l1) t r b1, SameLength' l r,
      SameLength' r l, SameLength' r (LabelsOf l),
      SameLength' (LabelsOf l) r, HAllTaggedLV l, HAllTaggedLV r) =>
     Record t -> [Record l1] -> [Record t]
semiJoinTOnR _ [] = []
semiJoinTOnR t1 [t2] =
    if tIntersectEq t1 t2 then [t1]
                          else []
semiJoinTOnR t1 (t2:r2) =
    if tIntersectEq t1 t2 then [t1]
                          else semiJoinTOnR t1 r2

tIntersectEq
  :: (Eq (HList l), HRearrange3 (LabelsOf l) r l,
      HLabelSet (LabelsOf l), HLabelSet (LabelsOf r),
      H2ProjectByLabels (LabelsOf t) l1 l b,
      H2ProjectByLabels (LabelsOf l1) t r b1, SameLength' l r,
      SameLength' r l, SameLength' r (LabelsOf l),
      SameLength' (LabelsOf l) r, HAllTaggedLV l, HAllTaggedLV r) =>
     Record t -> Record l1 -> Bool
tIntersectEq t1 t2 = hProjectByLabels ( labelsOf t1 ) t2 `unordRecEq` hProjectByLabels ( labelsOf t2 ) t1

{- | Performs a semi-difference of the first given relation against the second
given relation.

Aka. antijoin:

/Also known, a trifle inappropriately, as antijoin./

   - Chris Date 2011, SQL and Relational Theory 2nd ed. p. 133

See 'Database.HaskRel.Relational.Expression.notMatching'.
-}
notMatching
  :: (Eq (HList l), Ord (HList t), HRearrange3 (LabelsOf l) r l,
      HLabelSet (LabelsOf l), HLabelSet (LabelsOf r),
      H2ProjectByLabels (LabelsOf t) l1 l b,
      H2ProjectByLabels (LabelsOf l1) t r b1, SameLength' l r,
      SameLength' r l, SameLength' r (LabelsOf l),
      SameLength' (LabelsOf l) r, HAllTaggedLV l, HAllTaggedLV r) =>
     Relation t -> Relation l1 -> Relation t
notMatching = semiDiff

{- | Alias of 'notMatching'. See
'Database.HaskRel.Relational.Expression.semiDiff'. -}
semiDiff
  :: (Eq (HList l), Ord (HList t), HRearrange3 (LabelsOf l) r l,
      HLabelSet (LabelsOf l), HLabelSet (LabelsOf r),
      H2ProjectByLabels (LabelsOf t) l1 l b,
      H2ProjectByLabels (LabelsOf l1) t r b1, SameLength' l r,
      SameLength' r l, SameLength' r (LabelsOf l),
      SameLength' (LabelsOf l) r, HAllTaggedLV l, HAllTaggedLV r) =>
     Relation t -> Relation l1 -> Relation t
semiDiff r1 r2 = 
    let tl2 = toList r2
     in fromList $ Data.Set.foldr (\t1 b -> semiDiffTOnR t1 tl2 ++ b) [] r1

semiDiffTOnR
  :: (Eq (HList l), HRearrange3 (LabelsOf l) r l,
      HLabelSet (LabelsOf l), HLabelSet (LabelsOf r),
      H2ProjectByLabels (LabelsOf t) l1 l b,
      H2ProjectByLabels (LabelsOf l1) t r b1, SameLength' l r,
      SameLength' r l, SameLength' r (LabelsOf l),
      SameLength' (LabelsOf l) r, HAllTaggedLV l, HAllTaggedLV r) =>
     Record t -> [Record l1] -> [Record t]
semiDiffTOnR t1 [] = [t1]
semiDiffTOnR t1 [t2] =
    if tIntersectEq t1 t2 then []
                          else [t1]
semiDiffTOnR t1 (t2:r2) =
    if tIntersectEq t1 t2 then []
                          else semiDiffTOnR t1 r2



{- Image Relations

Definition: Let relations r1 and r2 be joinable (i.e., such that attributes with
the same name are of the same type); let t1 be a tuple of r1; let t2 be a tuple
of r2 that has the same values for those common attributes as tuple t1 does; let
relation r3 be that restriction of r2 that contains all and only such tuples t2;
and let relation r4 be the projection of r3 on all but those common
attributes. Then r4 is the image relation (with respect to r2) corresponding to
t1.  - Chris J. Date, SQL and Relational Theory 2nd ed. p 136
-}

-- Project tuple on relation heading
projectTOnR :: forall a b ls t proxy .
     (HLabelSet (LabelsOf a), H2ProjectByLabels ( LabelsOf ls ) t a b,
      HAllTaggedLV a) =>
     Record t -> Relation ls -> Record a
projectTOnR t _ = hProjectByLabels ( Proxy :: Proxy ( LabelsOf ls ) ) t

-- Specialization of restriction that takes a relation and a tuple whose heading
-- is a (not neccesarily proper) subset of the heading of the relation, and
-- restricts the relation by the intersection of attributes of the relation and
-- tuple.
restrictByRTuple
  :: (Eq (HList l), HLabelSet (LabelsOf l),
      H2ProjectByLabels (LabelsOf l) t l b, HAllTaggedLV l) =>
     Relation t -> Record l -> Relation t
restrictByRTuple r t = Data.Set.filter ( ( t == ) . hProjectByLabels ( labelsOf t ) ) r

-- TODO: It would possibly be more performant and still correct to use partition
-- instead of filter, and feed the remainder part of the result of that back
-- into this function as parameter r.


{-| The image of a relation corresponding to an r-tuple.

An application of the first argument only, an r-tuple, to this function yields
what is known as the @!!@ operator in Tutorial D.

>>> let qtySum = Label::Label "qtySum"
>>> :{
    rPrint$ s'
            `project` (rHdr (sno))
            `extendA` (\ (image -> ii) ->
                            qtySum .=.
                                ( sum $ aggU $ ii ( sp' `project` (rHdr (sno,qty)) ) ) )
 :}

See 'Database.HaskRel.Relational.Expression.image'.
-}
image
  :: (Eq (HList l), Ord (HList r'), HLabelSet (LabelsOf l),
      HDeleteLabels (LabelsOf l1) r r',
      H2ProjectByLabels (LabelsOf l) r l b,
      H2ProjectByLabels (LabelsOf r) l1 l b1, HAllTaggedLV l) =>
     Record l1 -> Relation r -> Relation r'
image t r = projectAllBut (restrictByRTuple r $ projectTOnR t r) (labelsOf t)


extendAByImage
  :: (Eq (HList l), Ord v, Ord (HList l1), Ord (HList r'),
      HLabelSet (LabelsOf l), HLabelSet (Label t ': LabelsOf l1),
      HDeleteLabels (LabelsOf l1) r r',
      H2ProjectByLabels (LabelsOf l) r l b,
      H2ProjectByLabels (LabelsOf r) l1 l b1, HAllTaggedLV l1,
      HAllTaggedLV l) =>
     Relation r -> Relation l1
     -> (Relation r' -> Tagged t v) -> Relation (Tagged t v ': l1)
extendAByImage rel relP fOut = dExtendA relP $ fOut . ( `image` rel )

-- TODO: group/groupAllBut can most likely be optimized by folding over them in
-- a manner equivalent to this (somewhat) relational approach.

{-| Groups the given attributes of the given relation into a given new relation
valued attribute.

As the Tutorial D GROUP operator, not SQL GROUP BY.

See 'Database.HaskRel.Relational.Expression.group'.
-}
group
  :: (Eq (HList l), Ord v, Ord (HList l1), Ord (HList r'),
      HLabelSet (LabelsOf l), HLabelSet (Label t ': LabelsOf l1),
      HDeleteLabels ks r l1, HDeleteLabels (LabelsOf l1) r r',
      H2ProjectByLabels (LabelsOf l) r l b,
      H2ProjectByLabels (LabelsOf r) l1 l b1, HAllTaggedLV l1,
      HAllTaggedLV l) =>
     Relation r
     -> proxy ks -> (Relation r' -> Tagged t v) -> Relation (Tagged t v ': l1)
group rel attsIn = extendAByImage rel $ rel `projectAllBut` attsIn

{- TODO: That "group" supports both grouping into an RVA and grouping like group
by is a bit of a design accident, consider making group only accept a label, and
having a function that generalizes group that takes a function as group does
now. Further consider a method that just takes a relation and a function,
deriving the labels from the function result (I tried to do this but ran into
issues).
-}

{- | Groups the given relation on all but the given attributes into a given new
attribute.

See 'Database.HaskRel.Relational.Expression.groupAllBut'.
-}
groupAllBut
  :: (Eq (HList l), Ord v, Ord (HList l1), Ord (HList r'),
      HLabelSet (LabelsOf l), HLabelSet (LabelsOf l1),
      HLabelSet (Label t ': LabelsOf l1),
      HDeleteLabels (LabelsOf l1) t1 r', H2ProjectByLabels ls t1 l1 b2,
      H2ProjectByLabels (LabelsOf l) t1 l b,
      H2ProjectByLabels (LabelsOf t1) l1 l b1, HAllTaggedLV l1,
      HAllTaggedLV l) =>
     Relation t1
     -> proxy ls -> (Relation r' -> Tagged t v) -> Relation (Tagged t v ': l1)
groupAllBut rel attsIn = extendAByImage rel $ rel `project` attsIn

{-| Ungroups the given attribute of the given relation.

>>> let pq = (Label :: Label "pq")
>>> sp' == ungroup ( group sp' (rHdr (pno,qty)) (pq .=.)) pq
True

Note the difference to 'Database.HaskRel.Relational.Expression.ungroup', which
requires 'Database.HaskRel.Relational.Expression.rEq' for relational comparison.
-}
ungroup
  :: (Eq (HList l), Ord (HList (HAppendListR t1 t2)),
      HasField l1 (Record v) (Relation t), HRearrange3 (LabelsOf l) r l,
      HLabelSet (LabelsOf l), HLabelSet (LabelsOf t2),
      HLabelSet (LabelsOf r), HLabelSet (LabelsOf (HAppendListR t1 t2)),
      H2ProjectByLabels (LabelsOf t1) t l t2,
      H2ProjectByLabels (LabelsOf t) t1 r b,
      H2ProjectByLabels '[Label l1] v t3 t1, HAppendList t1 t2,
      SameLength' l r, SameLength' r l, SameLength' r (LabelsOf l),
      SameLength' (LabelsOf l) r, HAllTaggedLV t2, HAllTaggedLV l,
      HAllTaggedLV r, HAllTaggedLV (HAppendListR t1 t2)) =>
     Relation v -> Label l1 -> Relation (HAppendListR t1 t2)
ungroup r a = fromList $ Data.Set.foldr f [] r
  where f t b = joinTOnR (hDeleteAtLabel a t) (t .!. a) ++ b


extendByImage
  :: (Eq (HList l), Ord (HList r'2),
      Ord (HList (HAppendListR r r'1)),
      HLabelSet (LabelsOf (HAppendListR r r'1)),
      HLabelSet (LabelsOf (HAppendListR r' r)), HLabelSet (LabelsOf l),
      HDeleteLabels (LabelsOf r) r' r'1,
      HDeleteLabels (LabelsOf r') r1 r'2,
      H2ProjectByLabels (LabelsOf l) r1 l b,
      H2ProjectByLabels (LabelsOf r1) r' l b1, HAppendList r r'1,
      HAllTaggedLV l, HAllTaggedLV (HAppendListR r r'1),
      HAllTaggedLV (HAppendListR r' r)) =>
     Relation r1
     -> Relation r'
     -> (Relation r'2 -> Record r)
     -> Relation (HAppendListR r r'1)
extendByImage rel relP fOut = dExtend relP $ fOut . ( `image` rel )

{- | Summarize.

See 'Database.HaskRel.Relational.Expression.summarize'.
-}
summarize
  :: (Eq (HList l), Ord (HList r'), Ord (HList r'2),
      Ord (HList (HAppendListR r r'1)),
      HLabelSet (LabelsOf (HAppendListR r r'1)),
      HLabelSet (LabelsOf (HAppendListR r' r)), HLabelSet (LabelsOf l),
      HDeleteLabels ks r2 r', HDeleteLabels (LabelsOf r) r' r'1,
      HDeleteLabels (LabelsOf r') r1 r'2,
      H2ProjectByLabels (LabelsOf l) r1 l b,
      H2ProjectByLabels (LabelsOf r1) r' l b1, HAppendList r r'1,
      HAllTaggedLV l, HAllTaggedLV (HAppendListR r r'1),
      HAllTaggedLV (HAppendListR r' r)) =>
     Relation r1
     -> Relation r2
     -> proxy ks
     -> (Relation r'2 -> Record r)
     -> Relation (HAppendListR r r'1)
summarize relA relB attsIn f = extendByImage relA ( relB `projectAllBut` attsIn ) f

{- | Auto-summarization. A specialization of 'summarize' with the same source and
destination relation.

See 'Database.HaskRel.Relational.Expression.aSummarize'.
-}
aSummarize
  :: (Eq (HList l), Ord (HList r'), Ord (HList r'2),
      Ord (HList (HAppendListR r r'1)),
      HLabelSet (LabelsOf (HAppendListR r r'1)),
      HLabelSet (LabelsOf (HAppendListR r' r)), HLabelSet (LabelsOf l),
      HDeleteLabels ks r1 r', HDeleteLabels (LabelsOf r) r' r'1,
      HDeleteLabels (LabelsOf r') r1 r'2,
      H2ProjectByLabels (LabelsOf l) r1 l b,
      H2ProjectByLabels (LabelsOf r1) r' l b1, HAppendList r r'1,
      HAllTaggedLV l, HAllTaggedLV (HAppendListR r r'1),
      HAllTaggedLV (HAppendListR r' r)) =>
     Relation r1
     -> proxy ks
     -> (Relation r'2 -> Record r)
     -> Relation (HAppendListR r r'1)
aSummarize rel attsIn f = extendByImage rel ( rel `projectAllBut` attsIn ) f

{-
-- TODO: Needs work: >>> pt$ summarize' sp' (s' `project` (Proxy :: Labels '["sno"])) (\ (r :: Relation '[QTY, PNO]) -> qty .=. sum ( agg qty r ) .*. emptyRecord)   ==> Boom!
-- Although regular summarize suffers from the same issue, so it might not be a fixable issue:
-- pt$ summarize sp' (s' `project` (Proxy :: Labels '["sno"])) (Proxy :: Labels '["qty"]) (\(r :: Relation '[QTY,PNO]) -> qty .=. sum ( agg qty r ) .*. emptyRecord)
-- Boom!

{- | Summarize on the arguments of the function.

See 'Database.HaskRel.Relational.Expression.summarize''.
-}
summarize'
  :: (Eq (HList l), Ord (HList r'), Ord (HList r'2),
      Ord (HList (HAppendListR r r'1)),
      HLabelSet (LabelsOf (HAppendListR r r'1)),
      HLabelSet (LabelsOf (HAppendListR r' r)), HLabelSet (LabelsOf l),
      HDeleteLabels (LabelsOf r'2) r2 r',
      HDeleteLabels (LabelsOf r) r' r'1,
      HDeleteLabels (LabelsOf r') r1 r'2,
      H2ProjectByLabels (LabelsOf l) r1 l b,
      H2ProjectByLabels (LabelsOf r1) r' l b1, HAppendList r r'1,
      HAllTaggedLV l, HAllTaggedLV (HAppendListR r r'1),
      HAllTaggedLV (HAppendListR r' r)) =>
     Relation r1
     -> Relation r2
     -> (Relation r'2 -> Record r)
     -> Relation (HAppendListR r r'1)
summarize' relA relB f = extendByImage relA ( relB `projectAllBut` ( labelsOfRArgs f ) ) f

{- | Auto-summarization on the argument types of the function.

See 'Database.HaskRel.Relational.Expression.aSummarize''.
-}
aSummarize'
  :: (Eq (HList l), Ord (HList r'), Ord (HList r'2),
      Ord (HList (HAppendListR r r'1)),
      HLabelSet (LabelsOf (HAppendListR r r'1)),
      HLabelSet (LabelsOf (HAppendListR r' r)), HLabelSet (LabelsOf l),
      HDeleteLabels (LabelsOf r'2) r1 r',
      HDeleteLabels (LabelsOf r) r' r'1,
      HDeleteLabels (LabelsOf r') r1 r'2,
      H2ProjectByLabels (LabelsOf l) r1 l b,
      H2ProjectByLabels (LabelsOf r1) r' l b1, HAppendList r r'1,
      HAllTaggedLV l, HAllTaggedLV (HAppendListR r r'1),
      HAllTaggedLV (HAppendListR r' r)) =>
     Relation r1
     -> (Relation r'2 -> Record r) -> Relation (HAppendListR r r'1)
aSummarize' rel f = extendByImage rel ( rel `projectAllBut` ( labelsOfRArgs f ) ) f

labelsOfRArgs :: (Relation l -> x) -> Proxy (LabelsOf l)
labelsOfRArgs f = Proxy
-}


labels :: Relation '[Tagged a a', Tagged b b'] -> (Label a, Label b)
labels rel = undefined

-- TODO: Instead of "~z" the temporary label should be named something like x ++
-- "~" ++ y
-- TODO: Can a signature be defined? Just inferring it gives one that doesn't
-- compile, which means that we must rely on "labels".
{-| Calculates the transitive closure corresponding to a given relation.

See 'Database.HaskRel.Relational.Expression.tClose'.
-}
tClose xy =
  let (x,y) = labels xy
      r1 = xy `renameA` (y `as` (Label::Label "~z"))
      r2 = xy `renameA` (x `as` (Label::Label "~z"))
      r3 = ( r1 `nJoin` r2 ) `project` (x .*. y .*. HNil)
      r4 = xy `union` r3
   in if r4 == xy then r4
      else tClose r4

{- See [1] p 160. A transcription of TCLOSE seems to be:

tClose' xy =
  let r1 = xy `renameA` ((Label::Label "y") `as` (Label::Label "~z"))
      r2 = xy `renameA` ((Label::Label "x") `as` (Label::Label "~z"))
      r3 = ( r1 `nJoin` r2 ) `project` ((Label::Label "x") .*. (Label::Label "y") .*. HNil)
      r4 = xy `union` r3
   in if r4 == xy then r4
      else tClose' r4

But it's then used against PP, whose labels aren't X and Y. It can't be
positional? This is what tClose does, which isn't an issue since it is
subsequently fed through a commutative function (natural join), although it is
definitely unfortunate.
-}



-- Supporting, non-relational functions:

-- | Gives the cardinality of the argument.
count :: Set a -> Int
count = Data.Set.size

-- | Gives whether the given argument is empty or not.
isEmpty :: Set a -> Bool
isEmpty = Data.Set.null

{- | Tests whether the second argument is a proper subset of the first.

See 'Database.HaskRel.Relational.Expression.isProperSubsetOf'.
-}
isProperSubsetOf
  :: (Ord (HList l), HRearrange3 (LabelsOf l) r l,
      HLabelSet (LabelsOf l), SameLength' r l,
      SameLength' r (LabelsOf l), SameLength' l r,
      SameLength' (LabelsOf l) r) =>
     Relation l -> Relation r -> Bool
isProperSubsetOf l r = Data.Set.isProperSubsetOf l ( relRearrange r )

{- | Tests whether the second argument is a subset of the first.

See 'Database.HaskRel.Relational.Expression.isSubsetOf'.
-}
isSubsetOf
  :: (Ord (HList l), HRearrange3 (LabelsOf l) r l,
      HLabelSet (LabelsOf l), SameLength' r l,
      SameLength' r (LabelsOf l), SameLength' l r,
      SameLength' (LabelsOf l) r) =>
     Relation l -> Relation r -> Bool
isSubsetOf l r = Data.Set.isSubsetOf l ( relRearrange r )

{- | Right-fold of an attribute of a relation (although a "right" fold doesn't
make sense in the context of the relational model). The value of the third
argument, 'att', is not used.

See 'Database.HaskRel.Relational.Expression.rafoldr'.
-}
rafoldr
  :: (Foldable t, HasField l a b1) =>
     (b1 -> b -> b) -> b -> Label l -> t a -> b
rafoldr f b a r = foldr ( f . (.!. a) ) b r


{- | Attribute value aggregation, a specialization of 'rafoldr' that aggregates
the values of a single attribute into a list of the values the attribute type
wraps.

The value of the first argument, 'att', is not used.

>>> sum $ agg qty sp'
3100

See 'Database.HaskRel.Relational.Expression.agg'.
-}
agg :: (Foldable t, HasField l a a1) => Label l -> t a -> [a1]
agg = rafoldr (:) []


unwrapUnary :: Record '[Tagged t t1] -> t1
unwrapUnary ( Record (Tagged v `HCons` HNil) ) = v

{- | Right-fold of the attribute of an unary relation.

See 'Database.HaskRel.Relational.Expression.rafoldrU'.
-}
rafoldrU :: Foldable t => (b1 -> b -> b) -> b -> t (Record '[Tagged t1 b1]) -> b
rafoldrU f b r = foldr ( f . unwrapUnary ) b r

{- | Aggregation of the single attribute of an unary relation. A specialization of
'agg', and thus in turn of 'rafoldr', that aggregates the single attribute of an
unary relation, without requiring the name of that attribute.

>>> sum $ aggU $ sp' `project` (rHdr (qty))
1000

See 'Database.HaskRel.Relational.Expression.aggU'.
-}
aggU :: Foldable t => t (Record '[Tagged t1 a]) -> [a]
aggU = rafoldrU (:) []
