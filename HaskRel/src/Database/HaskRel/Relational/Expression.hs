{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, FlexibleInstances, UndecidableInstances, PolyKinds #-}
{-# LANGUAGE DataKinds, TypeOperators #-}

{-|
Module      : Database.HaskRel.Relational.Expression
Description : Support for relational expressions
Copyright   : © Thor Michael Støre, 2015
License     : GPL v2 without "any later version" clause
Maintainer  : thormichael át gmail døt com
Stability   : experimental

"Database.HaskRel.Relational.Algebra" and
"Database.HaskRel.Relational.Assignment" defines the functions of the relational
algebra and relational assignment, but in order to keep pertinent concerns
separated it only defines functions for relational operations upon values, not
relation variables. This module redefines those functions, generalizing them
such that they operate upon relation values, relation variables and relational
IO (relational expressions that build upon relvars), and also adds 'HFWPresent'
instances for relational IO.

Running "examples/suppliersPartsExample.sh" starts a GHCi session where these
examples can be run.
-}
module Database.HaskRel.Relational.Expression (
  {- * Functions defined in accordance with monadic operators of relational
       theory -}
  {-| (Not to be confused with Haskell monads.) -}
  -- ** The monadic operator class
  MonOp (monOp), MonOpRes, MonOpArg,
  -- ** The functions defined as monadic operators in the relational algebra
  rename, extend, restrict, project, projectAllBut,
  group, groupAllBut, ungroup,
  -- ** Supplementary functions
  -- *** Specializations of functions of the relational model, with relational closure
  -- **** Not part of relational theory
  dExtend, extendA, dExtendA, renameA, aSummarize, imageExtendL,
  -- *** Without relational closure
  image, member, notMember,
  -- **** Not part of relational theory
  rafoldr, rafoldrU, agg, aggU, count, isEmpty,
  rAgg, rAggU,
  {- * Functions defined in accordance with dyadic operators in relational
       theory -}
  -- ** The dyadic operator class
  DyaOp, DyaOpRes, DyaOpLeft, DyaOpRight, dyaOp,
  -- ** The functions defined as dyadic operators in the relational algebra
  naturalJoin, nJoin, times, matching, semiJoin, notMatching,
  semiDiff, union, dUnion, intersect, minus, xUnion, isProperSubsetOf,
  isSubsetOf, rEq,
  -- ** Somewhat deprecated operators of the relational algebra
  summarize,
  {- ** Specializations of functions of the relational model, with relational
     closure -}
  -- **** Not part of relational theory
  interJoin, iJoin,
  -- * Assignment functions
  -- ** The assignment operator class
  RelAssign (relAssign), RelAssignArg,
  -- ** The primitive assignment function
  assign,
  -- ** Specialized assignment functions
  insert, dInsert, delete, iDelete,
  -- update[A], updateAll[A] and deleteP only take relvars and optionally
  -- predicates as arguments, and not relation values, and are as such not
  -- re-defined in this module, but are re-exported for completeness
  Assignment.deleteP, Assignment.update, Assignment.updateAll,
  -- ** Further specialized and simplified forms of update
  -- **** Not part of relational theory
  Assignment.updateA, Assignment.updateAllA,
  -- ** Additional classes
  Algebra.NotEmpty
      ) where

import Data.Set (Set)
import qualified Data.Set (member, notMember)

import Data.HList.CommonMain

import Database.HaskRel.HFWTabulation
  ( HFWPresent, HListTypeSynonym, hfwPrint, hfwPrintTypedTS,
    printHRecSetTab, printHRecSetTabTyped, printHRecSetTabTypedTS )

import Database.HaskRel.Relational.Definition
import qualified Database.HaskRel.Relational.Algebra as Algebra
import qualified Database.HaskRel.Relational.Assignment as Assignment
import Database.HaskRel.Relational.Variable ( Relvar, readRelvar )


instance ( HFWPresent ( Relation r ) ) =>
         HFWPresent ( IO ( Relation r ) )
    where
      hfwPrint r = r >>= hfwPrint
      hfwPrintTypedTS ts r = r >>= hfwPrintTypedTS ts


-- | The class of relational monadic operators
class MonOp a where
    type MonOpRes a res :: *
    type MonOpRes a res = IO res
    type MonOpArg a :: *
    -- | Unary relational function application.
    monOp :: (MonOpArg a -> res) -> a -> MonOpRes a res
-- Alternatively, enforcing relational closure:
--    monOp :: (MonOpArg a -> (Relation res)) -> a -> (MonOpRes a (Relation res))
-- But it's convenient to also employ this for aggregating functions
-- TODO: This resulted in IO (IO a) at some point, should GHCi warn about that?
-- Or if main results in this, for that matter.

instance MonOp (Relation a) where
    type MonOpRes (Relation a) res = res
    type MonOpArg (Relation a) = Relation a
    monOp = id

instance MonOp (IO (Relation a)) where
    type MonOpArg (IO (Relation a)) = Relation a
    monOp = fmap

instance (Ord (HList b), Read (HList (RecordValuesR b)), RecordValues b,
          HMapAux HList TaggedFn (RecordValuesR b) b) =>
         MonOp (Relvar b) where
    type MonOpArg (Relvar b) = Relation b
    monOp f r = f <$> readRelvar r


{- By always resulting in IO we get a simpler variant. -}
class MonOp' a where
    type MonOpArg' a :: *
    monOp' :: (MonOpArg' a -> (Relation res)) -> a -> (IO (Relation res))

instance MonOp' (Relation a) where
    type MonOpArg' (Relation a) = Relation a
    monOp' = (pure .)

instance MonOp' (IO (Relation a)) where
    type MonOpArg' (IO (Relation a)) = Relation a
    monOp' = fmap

instance (Ord (HList b), Read (HList (RecordValuesR b)), RecordValues b,
          HMapAux HList TaggedFn (RecordValuesR b) b) =>
         MonOp' (Relvar b) where
    type MonOpArg' (Relvar b) = Relation b
    monOp' f r = f <$> readRelvar r

-- The "values in, value out" variation allows for the same functions to be used
-- inside the functions that rename and extends takes, but this form is
-- necessary in certain cases; see 'rename' below, and the TODO to fix this.



{-| Rename given attributes of a relation.

>>> let pnu = Label :: Label "pnu"
>>> let colour = Label :: Label "colour"
>>> rPrint$ p `rename` nAs( pno `as` pnu, color `as` colour )
┌─────┬───────┬────────┬────────┬────────┐
│ pnu │ pName │ colour │ weight │ city   │
╞═════╪═══════╪════════╪════════╪════════╡
│ P1  │ Nut   │ Red    │ 12 % 1 │ London │
...

Note that due to an implementation disorder this always results in an IO
operation, even on values. This is not an intentional limit and will hopefully
be removed in the future. If this is not acceptable (for instance inside
'extend' and 'restrict' functions), then one has to rely on 'renameA', which
renames a single attribute.
-}
rename r l = monOp' (\r' -> Algebra.rename r' l) r
{- TODO: Figure out why using monOp instead of monOp' breaks down with: Could not
deduce (MonOpRes a (Relation r) ~ MonOpRes a (Relation r0))

This is annoying, since applying it directly in GHCi works:

import qualified Database.HaskRel.Relational.Algebra as Algebra (rename, as)
let pnu = Label :: Label "pnu"
let colour = Label :: Label "colour"
rPrint$ monOp (\r' -> Algebra.rename r' ( nAs( pno `as` pnu, color `as` colour ) ) ) p 
-> Great success printed here.

This doesn't fly either:
rename' r l = monOp (\r' -> Algebra.rename' r' l) r

Still breaks down with a signature and AllowAmbiguousTypes:

rename' :: forall a (a1 :: [*]) tr (r :: [*]).
    (Ord (HList r), HLabelSet (LabelsOf r),
     HMapAux HList (Algebra.Relabel tr) a1 r, SameLength' a1 r,
     SameLength' r a1, HAllTaggedLV r, MonOp a,
     MonOpArg a ~ Relation a1) =>
    a -> tr -> MonOpRes a (Relation r)
rename' r l = monOp (\r' -> Algebra.rename r' l) r

Does this require injective type families? 'res' should be possible to deduce.
-}

{-| Renames one attribute.

>>> let sCity = Label :: Label "sCity"
>>> rPrint$ s `renameA` (city `as` sCity)
┌────────┬─────┬───────┬────────┐
│ sCity  │ sno │ sName │ status │
╞════════╪═════╪═══════╪════════╡
│ Athens │ S5  │ Adams │ 30     │
│ London │ S1  │ Smith │ 20     │
│ London │ S4  │ Clark │ 20     │
│ Paris  │ S2  │ Jones │ 10     │
│ Paris  │ S3  │ Blake │ 30     │
└────────┴─────┴───────┴────────┘

This only accepts a single pair of labels, the label to rename and the new label, in contrast to Tutorial D rename which takes a set of from-to pairs.

@renameA@ can, unlike 'rename', be used by 'restrict' and 'extend':

>>> :{
  do spx <- readRelvar sp
     rPrint$ sp `restrict` (\( image -> ii ) ->
                            count ( ii $ renameA (renameA spx (sno `as` sn)) (pno `as` pn) )
                            > 2)
:}
...
-}
renameA r ft = monOp (\r' -> Algebra.renameA r' ft) r


{-| Extends the given relation with the r-tuple resulting from the second
argument. Existing attributes with the same name will be replaced.

The simplest form (aside from a no-op), extend with one attribute:

>>> rPrint$ extend p (\ [pun|weight|] -> (Label :: Label "gmwt") .=. weight * 454 .*. emptyRecord )
┌──────────┬─────┬───────┬───────┬────────┬────────┐
│ gmwt     │ pno │ pName │ color │ weight │ city   │
╞══════════╪═════╪═══════╪═══════╪════════╪════════╡
│ 5448 % 1 │ P1  │ Nut   │ Red   │ 12 % 1 │ London │
...
│ 8626 % 1 │ P6  │ Cog   │ Red   │ 19 % 1 │ London │
└──────────┴─────┴───────┴───────┴────────┴────────┘

When replacing an attribute with extend one must take care not to cause a naming
collision. Using pattern matching with @case ... of ...@ one can pass values
from one context to another with Haskell tuples, and reuse variable names,
although this does require some duplication. It is also possible to use @pun@ to
build the output, instead of @.*.@ and @emptyRecord@. Add one attribute, replace
another:

>>> rPrint$ extend p (\[pun|weight color|] -> case (weight + 10, color ++ "-ish") of (weight, altColor) -> [pun|weight altColor|])
┌────────┬───────────┬─────┬───────┬───────┬────────┐
│ weight │ altColor  │ pno │ pName │ color │ city   │
╞════════╪═══════════╪═════╪═══════╪═══════╪════════╡
│ 22 % 1 │ Blue-ish  │ P5  │ Cam   │ Blue  │ Paris  │
...
│ 29 % 1 │ Red-ish   │ P6  │ Cog   │ Red   │ London │
└────────┴───────────┴─────┴───────┴───────┴────────┘

Lining this up with the @EXTEND@ operator of Tutorial D, we can imagine @ case
(a, b) of (a', b') @ as a form of @ { a' := a , b' := b } @ (though we can
hardly equate them), while @pun@ is needed to unpack and pack this from and to
the r-tuples.

Also note that if an attribute is replaced then the cardinality of the result
will be equal or lower than that of the argument.

>>> count sp
12
>>> count $ sp `extend` (\_ -> sno .=. "S0" .*. pno .=. "P0" .*. emptyRecord)
4

It is also notable that since HaskRel is not based on SQL but on relational
theory as defined by Chris Date et al. today, and explicitly does not have
support for nulls and outer joins (as specified in [1] chapter 4), @extend@ is
employed to assemble the information SQL assembles with @OUTER JOIN@. The
following command (a variant of the first query on [1] page 154) gives a result
that includes the information given by SQL @RIGHT OUTER JOIN@:

>>> :{
do sp' <- readRelvar sp
   rPrint$ extend s (\ (image -> ii) -> pq .=. ii sp' .*. emptyRecord)
:}
┌───────────────┬─────┬───────┬────────┬────────┐
│ pq            │ sno │ sName │ status │ city   │
╞═══════════════╪═════╪═══════╪════════╪════════╡
│ ┌─────┬─────┐ │ S5  │ Adams │ 30     │ Athens │
│ │ pno │ qty │ │     │       │        │        │
│ ╞═════╪═════╡ │     │       │        │        │
│ └─────┴─────┘ │     │       │        │        │
│ ┌─────┬─────┐ │ S1  │ Smith │ 20     │ London │
│ │ pno │ qty │ │     │       │        │        │
│ ╞═════╪═════╡ │     │       │        │        │
│ │ P1  │ 300 │ │     │       │        │        │
│ │ P2  │ 200 │ │     │       │        │        │
...

See material about extend, image relations and RVAs in [1] chapter 7 for more.

Note the additional plumbing required to employ relvars inside the function
@extend@ takes; the function 'imageExtendL' has been created to provide a more
convenient way to express this, specializing upon @extend@ and 'image'.
-}
extend r f = monOp (`Algebra.extend` f) r

{- TODO: Is it possible to have a quasiquoter that lets one specify roughly the
same as the Tutorial D snippet mentioned in the documentation above? For
instance instead of:

(\[pun|weight color|] -> case (weight + 10, color ++ "-ish") of (weight, altColor) -> [pun|weight altColor|])

Having something like:
[rx| weight := weight + 10, altColor := color ++ "-ish" |]

That would also render extendA/dExtendA obsolete, tidying things up nicely.
-}

{-| Extends the given relation with the attribute resulting from the second
argument. If an attribute with the same name exists then it will be
replaced. This allows for the function of the second argument to be simpler than
for `extend`, which must return an r-tuple.

Where @c@ is an expression yielding a single attribute:

>>> extend a (\b -> c .*. emptyRecord)

Is equivalent to:

>>> extendA a (\b -> c)

The following has the same result as the first example for 'extend':

>>> let gmwt = (Label::Label "gmwt")
>>> rPrint$ extendA p (\[pun|weight|] -> gmwt .=. weight * 454)

Note that if one wants to alter the values of an existing attribute then one has
to avoid a name collision. The most convenient option will most often be having
a constructor function or label constant with a different name from the actual
label:

>>> let _weight a = (Label::Label "weight") .=. a
>>> rPrint$ extendA p (\[pun|weight|] -> _weight $ weight + 10)
-}
extendA r f = monOp (`Algebra.extendA` f) r
{-
Alternatively, perform the pattern matching without \"pun\", which allows
different variable names from the function names:

>>> rPrint$ extendA p' (\ ( hProjectByLabels' -> ( Record ( ( Tagged weight' :: Tagged "weight" Rational ) `HCons` HNil ) ) ) -> weight .=. weight' )
-}
-- TODO: The issue mentioned above could perhaps be solved by making pun create
-- variables that can serve both as data and as constructors. See also
-- https://hackage.haskell.org/package/HList/docs/Data-HList-Labelable.html

{-| Disjoint extension. Extends the given relation with the result of the second
argument, as 'extend', but without deleting any that exist.
-}
dExtend r f = monOp (`Algebra.dExtend` f) r

{-| Disjoint extension of a single attribute. Extends the given relation with the
result of the second argument, as 'extend', but without deleting any that
exist. @l@ cannot already have any attribute @e@.
-}
dExtendA r f = monOp (`Algebra.dExtendA` f) r


{-| Restricts the given relation according to the given predicate. Note that this
is the well known @WHERE@ operator of both SQL and Tutorial D, but since "where"
is a reserved keyword in Haskell it is named "restrict".

>>> rPrint$ p `restrict` (\[pun|weight|] -> weight < 17.5)
┌─────┬───────┬───────┬────────┬────────┐
│ pno │ pName │ color │ weight │ city   │
╞═════╪═══════╪═══════╪════════╪════════╡
│ P1  │ Nut   │ Red   │ 12 % 1 │ London │
...
│ P5  │ Cam   │ Blue  │ 12 % 1 │ Paris  │
└─────┴───────┴───────┴────────┴────────┘
-}
restrict r f = monOp (`Algebra.restrict` f) r


{-| Projects the given relation on the given heading.

>>> rPrint$ p `project` (rHdr (color,city))
┌───────┬────────┐
│ color │ city   │
╞═══════╪════════╡
│ Blue  │ Oslo   │
│ Blue  │ Paris  │
│ Green │ Paris  │
│ Red   │ London │
└───────┴────────┘
-}
project r a = monOp (`Algebra.project` a) r
-- TODO: project/projectAllBut must fail when it is given labels that don't
-- exist in the given relation, right now it accepts all sorts of nonesense
-- without complaining.

{-| Projects the given relation on the heading of said given relation minus the
given heading.

>>> rPrint$ p `projectAllBut` (rHdr (city))
┌─────┬───────┬───────┬────────┐
│ pno │ pName │ color │ weight │
╞═════╪═══════╪═══════╪════════╡
│ P1  │ Nut   │ Red   │ 12 % 1 │
│ P2  │ Bolt  │ Green │ 17 % 1 │
│ P3  │ Screw │ Blue  │ 17 % 1 │
│ P4  │ Screw │ Red   │ 14 % 1 │
│ P5  │ Cam   │ Blue  │ 12 % 1 │
│ P6  │ Cog   │ Red   │ 19 % 1 │
└─────┴───────┴───────┴────────┘
-}
projectAllBut r a = monOp (`Algebra.projectAllBut` a) r


{-| Groups the given attributes of the given relation into a given new relation
valued attribute.

As the Tutorial D GROUP operator, not SQL GROUP BY.

>>> let pq = (Label :: Label "pq")
>>> pt$ group sp (rHdr (pno,qty)) (pq .=.)
┌────────────────────────────────────┬───────────────┐
│ pq :: Relation '["pno","qty"]      │ sno :: String │
╞════════════════════════════════════╪═══════════════╡
│ ┌───────────────┬────────────────┐ │ S1            │
│ │ pno :: String │ qty :: Integer │ │               │
│ ╞═══════════════╪════════════════╡ │               │
│ │ P1            │ 300            │ │               │
...
└────────────────────────────────────┴───────────────┘

Note that the last argument is a function that tags any value with a label; an
attribute constructor. This is different from Tutorial D @GROUP@, which just
takes the equivalent of a label, but as long as an attribute constructor is
provided it will function the same way. Here is what we get if we aggregate the
relation we get as the argument to the receiving function with 'agg', instead of
just supplying an attribute constructor ("@(pq .=.)@" above):

>>> let qtys = (Label :: Label "qtys")
>>> pt$ group sp (rHdr (pno,qty))
                 ((qtys .=.) . agg qty)
┌───────────────────────────┬───────────────┐
│ qtys :: [Integer]         │ sno :: String │
╞═══════════════════════════╪═══════════════╡
│ [200]                     │ S3            │
│ [200,300,400]             │ S4            │
│ [300,200,400,200,100,100] │ S1            │
│ [300,400]                 │ S2            │
└───────────────────────────┴───────────────┘

"Get the quantities of items in stock for those suppliers that supply anything":

>>> pt$ group sp (rHdr (pno,qty)) ((qtys .=.) . sum . agg qty)
┌─────────────────┬───────────────┐
│ qtys :: Integer │ sno :: String │
╞═════════════════╪═══════════════╡
│ 200             │ S3            │
│ 700             │ S2            │
│ 900             │ S4            │
│ 1300            │ S1            │
└─────────────────┴───────────────┘

Note the difference between this and the example for 'image'. These last two
examples may be more clearly expressed with 'groupAllBut', since we then specify
the attributes that the resulting type will have.
-}
group rel attsIn fOut = monOp (\r' -> Algebra.group r' attsIn fOut) rel

{-| Groups the given relation on all but the given attributes into a given new
attribute.

>>> pt$ groupAllBut sp (rHdr (sno)) (pq .=.)

Same result as the first example for 'group'.

>>> pt$ groupAllBut sp (rHdr (sno)) ((qtys .=.) . sum . agg qty )

Same result as the last example for 'group'.
-}
groupAllBut rel attsIn attOut = monOp (\r' -> Algebra.groupAllBut r' attsIn attOut) rel

{-| Ungroups the given relation valued attribute of the given relation.

>>> sp `rEq` ungroup ( group sp (rHdr (pno,qty)) (pq .=.)) (undefined :: Label "pq")
True
-}
ungroup r a = monOp (`Algebra.ungroup` a) r


{-| Self-summarization, the special case of `summarize` where the source and
target relations is the same. This is closer to SQL GROUP BY.

>>> pt$ aSummarize sp (rHdr (pno,qty)) (\r -> qty .=. sum ( agg qty r ) .*. emptyRecord)

Same result as last example for 'group'.
-}
aSummarize rel attsIn fOut = monOp (\r' -> Algebra.aSummarize r' attsIn fOut) rel



{-| Gives the cardinality of the argument.

>>> count sp
12
-}
count :: (MonOp a, MonOpArg a ~ Set a1) => a -> MonOpRes a Int
count = monOp Algebra.count

{-| Gives whether the given argument is empty or not.

>>> isEmpty sp
False
-}
isEmpty :: (MonOp a, MonOpArg a ~ Set a1) => a -> MonOpRes a Bool
isEmpty = monOp Algebra.isEmpty

-- TODO: Figure out how all this will function when aggregating values into new
-- relations. That'll most likely happen in an extend clause, where IO support
-- is irrelevant.
{-| Right-fold of an attribute of a relation (although a "right" fold doesn't make
sense in the context of the relational model). Note that the value of the third
argument is not used and may be "undefined".

>>> rafoldr (+) 0 qty sp
3100
>>> rafoldr (*) 1 qty sp
27648000000000000000000000000
-}
rafoldr f b a r = monOp (Algebra.rafoldr f b a) r

{-| Attribute value aggregation, a specialization of 'rafoldr' that aggregates the
values of a single attribute into a list of the values the attribute type wraps.

Note that the value of the first argument is not used and may be "undefined".

>>> :{
 do sp' <- readRelvar sp
    putStrLn $ show $ sum $ agg qty sp'
:}
3100
-}
agg :: (Foldable t, HasField l a1 a2, MonOp a, MonOpArg a ~ t a1) =>
     Label l -> a -> MonOpRes a [a2]
agg = monOp . Algebra.agg

{-| Right-fold of the attribute of an unary relation.

>>> rafoldrU (+) 0 $ sp `project` (rHdr (qty))
1000
>>> rafoldrU (*) 1 $ sp `project` (rHdr (qty))
2400000000
-}
rafoldrU f b r = monOp (Algebra.rafoldrU f b) r

{-| Aggregation of the single attribute of an unary relation. A specialization of
'agg', and thus in turn of 'rafoldr', that aggregates the single attribute of a
unary relation, without requiring the name of that attribute.

>>> :{
 do sp' <- readRelvar sp
    putStrLn $ show $ sum $ aggU $ sp' `project` (rHdr (qty))
 :}
1000
-}
aggU :: (Foldable t, MonOp a,
      MonOpArg a ~ t (Record '[Tagged t1 a1])) =>
     a -> MonOpRes a [a1]
aggU = monOp Algebra.aggU

{-| Aggregates an attribute and applies a function to the result of that. A
specialization of `rafoldr`.

Note that the value of the first argument is not used and may be "undefined".

>>> rAgg qty sp sum
3100
-}
rAgg :: (Foldable t, HasField l a1 a2, MonOp a, MonOpArg a ~ t a1) =>
     Label l -> a -> ([a2] -> res) -> MonOpRes a res
rAgg a r f = monOp (f . Algebra.agg a) r

{-| Aggregates the attribute of an unary relation and applies a function to the
result of that. A specialization of `rafoldrU`.

>>> rAggU (sp `project` (rHdr (qty))) sum
1000
-}
rAggU r f = monOp (f . Algebra.aggU) r

-- TODO: Can this be made to work not just towards an IO(Relation a) but also
-- from it?
{-| The image of a relation corresponding to an r-tuple.

An application of the first argument only, an r-tuple, to this function yields
what is known as the @!!@ operator in Tutorial D.

>>> rPrint$ rTuple (sno .=. "S2", pno .=. "P1")
┌─────┬─────┐
│ sno │ pno │
├─────┼─────┤
│ S2  │ P1  │
└─────┴─────┘
>>> rPrint$ rTuple (sno .=. "S2", pName .=. "Nut")
┌─────┬───────┐
│ sno │ pName │
├─────┼───────┤
│ S2  │ Nut   │
└─────┴───────┘
>>> rPrint$ rTuple (sno .=. "S2", pno .=. "P1") `image` sp
┌─────┐
│ qty │
╞═════╡
│ 300 │
└─────┘
>>> rPrint$ rTuple (sno .=. "S2", pName .=. "Nut") `image` sp
┌─────┬─────┐
│ pno │ qty │
╞═════╪═════╡
│ P1  │ 300 │
│ P2  │ 400 │
└─────┴─────┘

Image relations give rise to summarization. Here is a form of the query, "get
the quantities of items in stock for all suppliers":

>>> :{
do sp' <- readRelvar sp
   rPrint$ s `project` (rHdr (sno))
             `extendA` (\ (image -> ii) ->
                          (Label::Label "qtySum") .=. ( sum $ agg qty $ ii sp' ) )
:}
┌────────┬─────┐
│ qtySum │ sno │
╞════════╪═════╡
│ 0      │ S5  │
│ 200    │ S3  │
│ 700    │ S2  │
│ 900    │ S4  │
│ 1300   │ S1  │
└────────┴─────┘

Note how view patterns are used to build the @ii@ operator, equivalent of
Tutorial D's @!!@ operator. An equivalent form of the lambda would be:

@
   (\\t -> (Label::Label "qtySum") .=. ( sum $ agg qty $ t \`image\` sp' ))
@

See `group` for a similar example.
-}
image t r = monOp (Algebra.image t) r
{- With FlexibleContexts and TypeFamilies the above could be expressed:
:{
do sp' <- readRelvar sp
   rPrint$ s `project` (rHdr (sno))
             `extendA` (\t -> ( let ii = image t
                                 in (Label::Label "qtySum") .=. ( sum $ agg qty $ ii sp' ) ) )
:}
-}


{-| Gives whether a given r-tuple is member of a given relation.

>>> member (rTuple(sno .=. "S3", qty .=. 200, pno .=. "P2")) sp
True
-}
member e r = monOp (Data.Set.member $ hRearrange' e) r

{-| Gives whether a given r-tuple is not a member of a given relation.

>>> notMember (rTuple(sno .=. "S3", qty .=. 200, pno .=. "P2")) sp
False
-}
notMember e r = monOp (Data.Set.notMember $ hRearrange' e) r



{- TODO: Is there a way to cut down on the number of instances? Building the
dyadic operator class directly on the monadic one doesn't work, the following
dyaOp'' definition compiles, but using it with nJoin'' breaks down in ambiguity.

dyaOp''
  :: (MonOp a, MonOp a1, MonOpRes a1 res1 ~ (MonOpArg a -> res)) =>
     (MonOpArg a1 -> res1) -> a1 -> a -> MonOpRes a res
dyaOp'' f r1 r2 = monOp ( monOp f r1 ) r2

-- nJoin'' :: ???
nJoin'' r1 r2 = dyaOp'' Algebra.nJoin r1 r2
-}

-- | The class of relational dyadic operators
class DyaOp a b where
    type DyaOpRes a b res :: *
    type DyaOpRes a b res = IO res
    type DyaOpLeft  a :: *
    type DyaOpRight b :: *
    dyaOp' :: (DyaOpLeft a -> DyaOpRight b -> res) -> a -> b -> DyaOpRes a b res

instance DyaOp (Relation a) (Relation b) where
    type DyaOpRes (Relation a) (Relation b) res = res
    type DyaOpLeft  (Relation a) = Relation a
    type DyaOpRight (Relation b) = Relation b
    dyaOp' = id

instance DyaOp (IO (Relation a)) (Relation b) where
    type DyaOpLeft  (IO (Relation a)) = Relation a
    type DyaOpRight (Relation b) = Relation b
    dyaOp' f r1 r2 = (`f` r2) <$> r1

instance DyaOp (Relation a) (IO (Relation b)) where
    type DyaOpLeft  (Relation a) = Relation a
    type DyaOpRight (IO (Relation b)) = Relation b
    dyaOp' f r1 r2 = f r1 <$> r2

instance DyaOp (IO (Relation a)) (IO (Relation b)) where
    type DyaOpLeft  (IO (Relation a)) = Relation a
    type DyaOpRight (IO (Relation b)) = Relation b
    dyaOp' f r1 r2 = f <$> r1 <*> r2

instance (Ord (HList b), Read (HList (RecordValuesR b)), RecordValues b,
          HMapAux HList TaggedFn (RecordValuesR b) b) =>
         DyaOp (Relation a) (Relvar b) where
    type DyaOpLeft  (Relation a) = Relation a
    type DyaOpRight (Relvar b) = Relation b
    dyaOp' f r1 r2 = f r1 <$> readRelvar r2

instance (Ord (HList a), Read (HList (RecordValuesR a)), RecordValues a,
          HMapAux HList TaggedFn (RecordValuesR a) a) =>
         DyaOp (Relvar a) (Relation b) where
    type DyaOpLeft  (Relvar a) = Relation a
    type DyaOpRight (Relation b) = Relation b
    dyaOp' f r1 r2 = (`f` r2) <$> readRelvar r1

instance (Ord (HList a), Read (HList (RecordValuesR a)), RecordValues a,
          HMapAux HList TaggedFn (RecordValuesR a) a) =>
         DyaOp (Relvar a) (IO (Relation b)) where
    type DyaOpLeft  (Relvar a) = Relation a
    type DyaOpRight (IO (Relation b)) = Relation b
    dyaOp' f r1 r2 = f <$> readRelvar r1 <*> r2

instance (Ord (HList b), Read (HList (RecordValuesR b)), RecordValues b,
          HMapAux HList TaggedFn (RecordValuesR b) b) =>
         DyaOp (IO (Relation a)) (Relvar b) where
    type DyaOpLeft (IO (Relation a)) = Relation a
    type DyaOpRight (Relvar b)     = Relation b
    dyaOp' f r1 r2 = f <$> r1 <*> readRelvar r2

instance (Ord (HList a), Ord (HList b), Read (HList (RecordValuesR a)),
          Read (HList (RecordValuesR b)), RecordValues a, RecordValues b,
          HMapAux HList TaggedFn (RecordValuesR a) a,
          HMapAux HList TaggedFn (RecordValuesR b) b) =>
         DyaOp (Relvar a) (Relvar b) where
    type DyaOpLeft  (Relvar a) = Relation a
    type DyaOpRight (Relvar b) = Relation b
    dyaOp' f r1 r2 = f <$> readRelvar r1 <*> readRelvar r2

-- TODO: Polyvariadic forms of the dyadic functions.

-- The algebra functions do relRearrange themselves, so don't do that in the class function but define a separate function for that. This is the only reason for this, so there's no need for the same with monOp, that can be exported as-is.

{-| Binary relational function application.

>>> let newSups = ( relation [rTuple (sno .=. "S6", sName .=. "Nena", city .=. "Berlin", status .=. 40)] )
>>> dyaOp (/=) s newSups
True
-}
dyaOp :: (Ord (HList l), HRearrange3 (LabelsOf l) r l,
          HLabelSet (LabelsOf l), SameLength' r l,
          SameLength' r (LabelsOf l), SameLength' l r,
          SameLength' (LabelsOf l) r, DyaOp a b,
          DyaOpRight b ~ Relation r) =>
     (DyaOpLeft a -> Relation l -> res) -> a -> b -> DyaOpRes a b res
dyaOp f r1 r2 = dyaOp' (\r1' r2' -> f r1' ( relRearrange r2' )) r1 r2

{-| Relational equality.

>>> sp `rEq` (relation [rTuple(sno .=. "S2", qty .=. 400, pno .=. "P2")])
False
-}
rEq :: (Ord (HList l), HRearrange3 (LabelsOf l) r l,
        HLabelSet (LabelsOf l), SameLength' r l,
        SameLength' r (LabelsOf l), SameLength' l r,
        SameLength' (LabelsOf l) r, DyaOp a b,
        DyaOpLeft a ~ Relation l, DyaOpRight b ~ Relation r) =>
     a -> b -> DyaOpRes a b Bool
rEq = dyaOp (==)
{- TODO: Oh great, now I need an IO version of restrict. This is tricky, my first
thought is that a predicate for 'filter' will still take a regular value as
before but may result in an IO Bool instead of Bool, so perhaps you need to do
something like :: ( a -> f b ) -> f ( a -> b )
-}

{-| The natural join of the two given relations.

>>> rPrint$ sp `naturalJoin` s
┌─────┬─────┬─────┬───────┬────────┬────────┐
│ sno │ pno │ qty │ sName │ status │ city   │
╞═════╪═════╪═════╪═══════╪════════╪════════╡
│ S1  │ P1  │ 300 │ Smith │ 20     │ London │
...
│ S4  │ P5  │ 400 │ Clark │ 20     │ London │
└─────┴─────┴─────┴───────┴────────┴────────┘
-}
naturalJoin r1 r2 = dyaOp' Algebra.naturalJoin r1 r2

-- | Alias of 'naturalJoin'.
nJoin r1 r2 = dyaOp' Algebra.nJoin r1 r2


{-| The cartesian product of two relations. A specialized natural join; the
natural join between two relations with disjoint headings.

>>> rPrint$ ( sp `projectAllBut` (rHdr (city)) ) `times` ( s `projectAllBut` (rHdr (city)) )
...
    No instance for (Fail (DuplicatedLabel (Label "sno")))
      arising from a use of ‘times’

>>> rPrint$ ( sp `projectAllBut` (rHdr (sno)) ) `times` ( s `projectAllBut` (rHdr (sno)) )
┌─────┬─────┬───────┬────────┬────────┐
│ pno │ qty │ sName │ status │ city   │
╞═════╪═════╪═══════╪════════╪════════╡
│ P1  │ 300 │ Adams │ 30     │ Athens │
│ P1  │ 300 │ Blake │ 30     │ Paris  │
...
│ P6  │ 100 │ Jones │ 10     │ Paris  │
│ P6  │ 100 │ Smith │ 20     │ London │
└─────┴─────┴───────┴────────┴────────┘
-}
times r1 r2 = dyaOp' Algebra.times r1 r2

{-| The natural join between two relations with intersecting headings. A
specialized natural join.

A join upon relations r1, r2 where the intersection of the heading of r1 and of
r2 is not empty; the headings are not disjoint. This is a complement of 'times'
within natural join; all values that @naturalJoin@ accepts as operands and
@times@ does not are accepted by @interJoin@, and vice-versa. The name is what I
quickly settled on, suggestions for a better one would be welcome.
(Attribute-Intersecting Natural Join is another candidate.)

This function doesn't have a specific identity value, although it holds that
@r \`interJoin\` r = r@

>>> rPrint$ ( sp `projectAllBut` (rHdr (sno)) ) `interJoin` ( s `projectAllBut` (rHdr (sno)) )
...
    Overlapping instances for NotEmpty '[]
      arising from a use of ‘interJoin’

>>> rPrint$ sp `interJoin` s
┌─────┬─────┬─────┬───────┬────────┬────────┐
│ sno │ pno │ qty │ sName │ status │ city   │
╞═════╪═════╪═════╪═══════╪════════╪════════╡
│ S1  │ P1  │ 300 │ Smith │ 20     │ London │
│ S1  │ P2  │ 200 │ Smith │ 20     │ London │
...
│ S4  │ P4  │ 300 │ Clark │ 20     │ London │
│ S4  │ P5  │ 400 │ Clark │ 20     │ London │
└─────┴─────┴─────┴───────┴────────┴────────┘
-}
interJoin r1 r2 = dyaOp' Algebra.interJoin r1 r2
-- | Alias of 'interJoin'
iJoin r1 r2 = dyaOp' Algebra.iJoin r1 r2


{- | Extends the first given relation with an attribute resulting from imaging
each tuple of said relation against the second given relation. The following
command gives a result that includes the information given by SQL @RIGHT OUTER
JOIN@:

>>> rPrint$ imageExtendL s sp pq
┌───────────────┬─────┬───────┬────────┬────────┐
│ pq            │ sno │ sName │ status │ city   │
╞═══════════════╪═════╪═══════╪════════╪════════╡
│ ┌─────┬─────┐ │ S5  │ Adams │ 30     │ Athens │
│ │ pno │ qty │ │     │       │        │        │
│ ╞═════╪═════╡ │     │       │        │        │
│ └─────┴─────┘ │     │       │        │        │
│ ┌─────┬─────┐ │ S1  │ Smith │ 20     │ London │
│ │ pno │ qty │ │     │       │        │        │
│ ╞═════╪═════╡ │     │       │        │        │
│ │ P1  │ 300 │ │     │       │        │        │
│ │ P2  │ 200 │ │     │       │        │        │
...

See also 'extend', which this function specializes, and 'image', which it uses
to perform this specialization.
-}
imageExtendL
  :: (Eq (HList l), Ord (HList l1), Ord (HList r'),
      HLabelSet (LabelsOf l), HLabelSet (Label t ': LabelsOf l1),
      HDeleteLabels (LabelsOf l1) r r',
      H2ProjectByLabels (LabelsOf l) r l b1,
      H2ProjectByLabels (LabelsOf r) l1 l b2, HAllTaggedLV l1,
      HAllTaggedLV l, DyaOp a b, DyaOpLeft a ~ Set (Record l1),
      DyaOpRight b ~ Relation r) =>
     a
     -> b
     -> Label t
     -> DyaOpRes a b (Set (Record (Tagged t (Relation r') ': l1)))
imageExtendL r1 r2 l = dyaOp' (\r1 r2 -> Algebra.imageExtendL r1 r2 l) r1 r2


{-| The semi-join of the first given relation against the second given relation.

>>> rPrint$ s `matching` sp
┌─────┬───────┬────────┬────────┐
│ sno │ sName │ status │ city   │
╞═════╪═══════╪════════╪════════╡
│ S1  │ Smith │ 20     │ London │
│ S2  │ Jones │ 10     │ Paris  │
│ S3  │ Blake │ 30     │ Paris  │
│ S4  │ Clark │ 20     │ London │
└─────┴───────┴────────┴────────┘
-}
matching r1 r2 = semiJoin r1 r2

-- | Alias of 'matching'.
semiJoin r1 r2 = dyaOp' Algebra.semiJoin r1 r2


{-| The semi-difference of the first given relation against the second given
relation. Aka. antijoin.

>>> rPrint$ s `notMatching` sp
┌─────┬───────┬────────┬────────┐
│ sno │ sName │ status │ city   │
╞═════╪═══════╪════════╪════════╡
│ S5  │ Adams │ 30     │ Athens │
└─────┴───────┴────────┴────────┘
-}
notMatching r1 r2 = semiDiff r1 r2
{- Don't name this "antijoin":

Also known, a trifle inappropriately, as antijoin.
   - Chris Date 2011, SQL and Relational Theory 2nd ed. p. 133
-}

-- | Alias of 'notMatching'.
semiDiff r1 r2 = dyaOp' Algebra.semiDiff r1 r2


{-| The union of two relations.

>>> let newSups = ( relation [rTuple (sno .=. "S6", sName .=. "Nena", city .=. "Berlin", status .=. 40)] )
>>> rPrint$ s `union` newSups
┌─────┬───────┬────────┬────────┐
│ sno │ sName │ status │ city   │
╞═════╪═══════╪════════╪════════╡
│ S1  │ Smith │ 20     │ London │
│ S2  │ Jones │ 10     │ Paris  │
│ S3  │ Blake │ 30     │ Paris  │
│ S4  │ Clark │ 20     │ London │
│ S5  │ Adams │ 30     │ Athens │
│ S6  │ Nena  │ 40     │ Berlin │
└─────┴───────┴────────┴────────┘
-}
union r1 r2 = dyaOp' Algebra.union r1 r2

{-| The disjoint union between the relations. This is a union of disjoint
relations, where a runtime error is raised if the arguments are not disjoint.

>>> :{
  rPrint$ ( p' `project` (rHdr (city)) )
          `dUnion`
          ( s' `project` (rHdr (city)) )
:}
┌─*** Exception: Arguments to dUnion are not disjoint, intersection:
┌────────┐
│ city   │
╞════════╡
│ London │
│ Paris  │
└────────┘
-}
dUnion r1 r2 = dyaOp' Algebra.dUnion r1 r2

{-| The intersection of two relations.

Note how the name is different from Data.Set, where it is named
"intersection". This is due to it being referred to as "intersect" in material
describing the relational model; specifically named \"INTERSECT\" in Tutorial D.

>>> let sX = ( relation [rTuple (sno .=. "S2", sName .=. "Jones", city .=. "Paris", status .=. 10), rTuple (sno .=. "S6", sName .=. "Nena", city .=. "Berlin", status .=. 40)] )
>>> rPrint$ s `intersect` sX
┌─────┬───────┬────────┬───────┐
│ sno │ sName │ status │ city  │
╞═════╪═══════╪════════╪═══════╡
│ S2  │ Jones │ 10     │ Paris │
└─────┴───────┴────────┴───────┘

Notably, for any given relation values r1 and r2 that are of the same type it
holds that:

@r1 \`intersect\` r2 == r1 \`naturalJoin\` r2@

Within relational theory the natural join generalizes as such both intersection
and cartesian product.
-}
intersect r1 r2 = dyaOp' Algebra.intersect r1 r2

{-| The difference of two relations.

The "minus" term is used in material describing relational theory; specifically
Tutorial D names the operator \"MINUS\".

>>> rPrint$ s `minus` sX
┌─────┬───────┬────────┬────────┐
│ sno │ sName │ status │ city   │
╞═════╪═══════╪════════╪════════╡
│ S1  │ Smith │ 20     │ London │
│ S3  │ Blake │ 30     │ Paris  │
│ S4  │ Clark │ 20     │ London │
│ S5  │ Adams │ 30     │ Athens │
└─────┴───────┴────────┴────────┘
-}
minus r1 r2 = dyaOp' Algebra.minus r1 r2

{- The difference of two relations. This differs from 'minus' in that the
attribute order of the second argument takes precedence. This function is as
such equal to 'minus' as far as relational theory is concerned, the difference
is on a lower level of abstraction.

>>> rPrint$ s `minus_` sX
┌─────┬───────┬────────┬────────┐
│ sno │ sName │ city   │ status │
╞═════╪═══════╪════════╪════════╡
│ S1  │ Smith │ London │ 20     │
│ S3  │ Blake │ Paris  │ 30     │
│ S4  │ Clark │ London │ 20     │
│ S5  │ Adams │ Athens │ 30     │
└─────┴───────┴────────┴────────┘
-}
-- minus_ r1 r2 = dyaOp' Algebra.minus_ r1 r2
-- TODO: Should minus_ be exported? I don't see a big reason to, Algebra.minus
-- is used directly by Assignment...


{-| Exclusive union, aka. symmetric difference

>>> rPrint$ s `xUnion` sX
┌─────┬───────┬────────┬────────┐
│ sno │ sName │ status │ city   │
╞═════╪═══════╪════════╪════════╡
│ S1  │ Smith │ 20     │ London │
│ S3  │ Blake │ 30     │ Paris  │
│ S4  │ Clark │ 20     │ London │
│ S5  │ Adams │ 30     │ Athens │
│ S6  │ Nena  │ 40     │ Berlin │
└─────┴───────┴────────┴────────┘
-}
xUnion r1 r2 = dyaOp' Algebra.xUnion r1 r2


{-| The summarization of the relations by the given function.

>>> let pct = Label :: Label "pct"
>>> rPrint$ summarize sp (s `project` (rHdr (sno))) (rHdr (pno)) (\r -> pct .=. count r .*. emptyRecord)
┌─────┬─────┐
│ pct │ sno │
╞═════╪═════╡
│ 0   │ S5  │
│ 1   │ S3  │
│ 2   │ S2  │
│ 3   │ S4  │
│ 6   │ S1  │
└─────┴─────┘

>>> rPrint$ summarize sp (s `project` (rHdr (sno))) (rHdr (qty)) (\r -> qty .=. sum ( agg qty r ) .*. emptyRecord)
┌──────┬─────┐
│ qty  │ sno │
╞══════╪═════╡
│ 0    │ S5  │
│ 200  │ S3  │
│ 700  │ S2  │
│ 900  │ S4  │
│ 1300 │ S1  │
└──────┴─────┘
-}
summarize r1 r2 attsIn fOut = dyaOp' (\r1' r2' -> Algebra.summarize r1' r2' attsIn fOut) r1 r2

{-| Tests whether the second argument is a proper subset of the first.

>>> let spX = relation [rTuple (sno .=. "S1", pno .=. "P4", qty .=. 200), rTuple (sno .=. "S2", pno .=. "P2", qty .=. 400)]
>>> spX `isProperSubsetOf` sp
True
-}
isProperSubsetOf r1 r2 = dyaOp' Algebra.isProperSubsetOf r1 r2

{-| Tests whether the second argument is a subset of the first.

>>> spX `isSubsetOf` sp
True
-}
isSubsetOf r1 r2 = dyaOp' Algebra.isProperSubsetOf r1 r2



-- | The class of relational assignment
class RelAssign a where
    type RelAssignArg a :: *
    -- | Relational IO function application.
    relAssign :: (RelAssignArg a -> IO ()) -> a -> IO ()

instance RelAssign (Relation a) where
    type RelAssignArg (Relation a) = (Relation a)
    relAssign = id

instance RelAssign (IO (Relation a)) where
    type RelAssignArg (IO (Relation a)) = Relation a
    relAssign = (=<<)

instance (Ord (HList b), Read (HList (RecordValuesR b)), RecordValues b,
          HMapAux HList TaggedFn (RecordValuesR b) b) =>
         RelAssign (Relvar b) where
    type RelAssignArg (Relvar b) = Relation b
    relAssign f r = f =<< readRelvar r


-- TODO: What is the correct fixity? (Should most likely be thought through for
-- the whole library.)
infix 1 `assign`
{-| Writes a relation value to a relvar file, replacing the existing value.

>>> assign s s'
Value assigned to SuppliersPartsDB/S.rv
-}
assign rv r = relAssign ( Assignment.assign rv ) r


infix 1 `insert`
{-| Inserts a relation into a relvar. This differs from SQLs INSERT; the relvar is
updated to the union of the relvar and the relation value given as arguments.

>>> let newSups = relation [rTuple (sno .=. "S6", sName .=. "Nena", city .=. "Berlin", status .=. 40)]
>>> insert s newSups
Inserted 1 of 1 tuples into SuppliersPartsDB/S.rv
>>> insert s newSups
Inserted 0 of 1 tuples into SuppliersPartsDB/S.rv
>>> rPrint$ s
┌─────┬───────┬────────┬────────┐
│ sno │ sName │ status │ city   │
╞═════╪═══════╪════════╪════════╡
...
│ S6  │ Nena  │ 40     │ Berlin │
└─────┴───────┴────────┴────────┘
-}
insert rv r = relAssign ( Assignment.insert rv ) r


infix 1 `dInsert`
{-| Disjoint insert. Closer to SQL INSERT, except that this will never insert a
duplicate tuple.

>>> dInsert sp $ relation [rTuple (sno .=. "S6", pno .=. "P7", qty .=. 99)]
Inserted 1 tuples into SuppliersPartsDB/SP.rv
>>> dInsert sp $ relation [rTuple (sno .=. "S6", pno .=. "P7", qty .=. 99), rTuple (sno .=. "S4", pno .=. "P4", qty .=. 300), rTuple (sno .=. "S7", pno .=. "P8", qty .=. 200)]
*** Exception: Unique constraint violation, tuples already present in SuppliersPartsDB/SP.rv:
┌─────┬─────┬─────┐
│ sno │ pno │ qty │
╞═════╪═════╪═════╡
│ S4  │ P4  │ 300 │
│ S6  │ P7  │ 99  │
└─────┴─────┴─────┘
-}
dInsert rv r = relAssign ( Assignment.dInsert rv ) r


infix 1 `delete`
{-| Deletes a specified subset of a relvar. Note that this is not SQL DELETE, but
instead a generalization thereof.

>>> delete s newSups
Deleted 1 tuples from SuppliersPartsDB/S.rv
-}
delete rv r = relAssign ( Assignment.delete rv ) r

infix 1 `iDelete`
{-| Performs an inclusive delete against a relvar. Also not SQL DELETE. This will
fail if the second argument is not a subset of the relation variable.

>>> iDelete sp $ relation [rTuple (sno .=. "S6", pno .=. "P7", qty .=. 99), rTuple (sno .=. "S4", pno .=. "P4", qty .=. 300), rTuple (sno .=. "S7", pno .=. "P8", qty .=. 200)]
*** Exception: Tuples not found in relvar SuppliersPartsDB/SP.rv:
┌─────┬─────┬─────┐
│ sno │ pno │ qty │
╞═════╪═════╪═════╡
│ S7  │ P8  │ 200 │
└─────┴─────┴─────┘
-}
iDelete rv r = relAssign ( Assignment.iDelete rv ) r
