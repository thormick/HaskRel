{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : Database.HaskRel.RelMonoid
Description : Monoid instances for relational functions
Copyright   : © Thor Michael Støre, 2015
License     : GPL v2 without "any later version" clause
Maintainer  : thormichael át gmail døt com
Stability   : experimental

Monoid instances for relational functions, defined on a best-effort attempt.

(Note that I don't know Haskell well enough to know whether there is actually a
proper solution for this or if this is explored elsewhere.)

__Synopsis:__ Proper monoid implementations of relational functions is not
possible as HaskRel is defined today. When using monoids with relational
functions one must manually rearrange one of the arguments. This may be an
impediment to the usefulness of monoids in combination with HaskRel, but I hope
the monoid implementations may be useful after all.

__Full description:__

Relational theory defines 10 algebraic operators that take two relations as
parameters. In Tutorial D they are named as follows:
UNION, D_UNION, XUNION, INTERSECT, MINUS, I_MINUS, JOIN, TIMES, MATCHING, NOT MATCHING

Of these union, xunion and join (natural join) would be candidates for defining
as monoids. They are associative, take two parameters and they have an identity
value. The issue is with how monoids are required to take and result in values of
the same type.

Regarding natural join the issue is that, by definition, relations of different
headings are of different types, which would preclude it as a monoid. On the
other hand, when viewing natural join in isolation this is a touch peculiar,
seeing how it can take any two relations of any types as arguments, and the
result of natural join is always acceptable as an argument to natural join
together with any other relation. If one therefore tries to see a classification
of types in light of the sets of values natural join takes as arguments and give
as a result one can only identify a single "relation type", which all relation
values of all headings are values of, and nothing in light of natural join gives
rise to any disctinction between relations of different headings. But there can
only be one classification of types in a type system, and the one which works for
relational theory does not behave this way. Natural join is therefore ineligible
as a monoid, which is quite a shame given that natural join is a very fundamental
operator of relational theory, and would be reasonable to consider as a monoid.

That leaves us with union and exclusive union, but here there are also issues. At
the outset it looks much more promising in that in relational theory the two
arguments it takes are required to be of the same type, but here we run into a
subtle issue when implementing it in one single system that manages multiple
layers of abstraction, as HaskRel does.

We see the issue when we consider the type of Database.HaskRel.Relational.Algebra.union:

>>> :t Database.HaskRel.Relational.Algebra.union
Database.HaskRel.Relational.Algebra.union
  :: (Ord (HList l), TagUntagFD a ta, TagUntagFD a1 l,
      HProject (HList a) (HList a1)) =>
     Relation' l -> Relation' ta -> Relation' l

'l' and 'ta' are defined as distinct types for Haskell, connected via the class
constraints TagUntagFD and HProject, and the user is therefore able to supply
values to the given function of types that are different according to Haskell,
but which are nonetheless identical in the aspects defined by the class
constraints, which aim to reflect relational theory. The way relational theory
defines these types there is no order to attributes of a relation.

This is, of course, in contrast to monoids:

@
class Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a
  	-- Defined in ‘GHC.Base’
@

Both mappend and mconcat require, quite reasonably, all arguments and results to
be of the same type. The deeper issue is that on the inside of a relational
database management system /implementation/ it must be possible to operate upon
records (or tuples, as defined in relational theory) both as a set and a list,
with subtle implications. In effect, HaskRel's implementations of functions that
take two relation as arguments (those defined as dyadic operators in relational
theory) are defined so that those two arguments are of different types as far as
the type system is concerned, but constrained to those that share those specific
properties that are relevant for the user to view them as "the same type".

This is therefore not merely a design accident; an implementation that manages
both aspects within one given type system must look more or less this way.
This is due to the fact that it must be possible to treat the records/tuples of
relation values as lists for purposes of presentation and representation.
Consider the following union, and the presentation of the resulting value:

>>> let r1 = relation [(Foo 1, Bar "foo")]
>>> let r2 = relation [(Bar "foo", Foo 2)]
>>> pt$ r1
┌───────────┬──────────────┐
│ Foo : Int │ Bar : String │
╞═══════════╪══════════════╡
| 1         │ "foo"        |
└───────────┴──────────────┘
>>> pt$ r2
┌──────────────┬───────────┐
│ Bar : String │ Foo : Int │
╞══════════════╪═══════════╡
| "foo"        │ 2         |
└──────────────┴───────────┘
>>> pt$ r1 `union` r2
┌───────────┬──────────────┐
│ Foo : Int │ Bar : String │
╞═══════════╪══════════════╡
| 1         │ "foo"        |
| 2         │ "foo"        |
└───────────┴──────────────┘

The order of the second argument is coerced to the order of the first by
Database.HaskRel.Relational.Algebra.union, (and hence to the exact same type as far as Haskell is
concerned), which is fine since in relational theory we consider the attributes
of a relation or tuple to be sets, and that there is no spoon/order. In contrast,
if the second row of the body of the tabluar representation of the result of @r1
\`union\` r2@ showed the same order as the representation of `r2` (Bar, Foo) and
not that of the row above then it would not be a very apt representation of the
result of the above union.

Therefore, as far as the `union` operator is concerned, and especially as far as
the user must be concerned, `r1` and `r2` must be considered "the same type". They
are only distinct as far as a system tasked with representing them as values
(certainly to a user as we see, but potentially also for other purposes, such as
for an efficient representation in computer memory) is concerned, but not as far
as a user should be concerned per-se. We see as such that the same values may be
appropriate to view as different but related types at different levels of
abstraction in a computer system as a whole.
-}

module Database.HaskRel.RelMonoid
       (Union(Union), getUnion, XUnion(XUnion), getXUnion, relRearrange) where

import Data.Set (Set, empty)
import qualified Data.Set as DS (union, intersection, difference)

import Database.HaskRel.Relational.Algebra (relRearrange)

{-|
>>> :t fbbRel
fbbRel :: Relation '[Foo, Bar, Baz]
>>> :t fbbRelX
fbbRelX :: Relation '[Bar, Foo, Baz]
>>> pt$ getUnion $ ( Union fbbRel ) `mappend` ( Union fbbRel2 )
┌───────────┬──────────────┬──────────────┐
│ Foo : Int │ Bar : String │ Baz : String │
╞═══════════╪══════════════╪══════════════╡
| 5         │ "asdf"       │ "qwer"       |
| 7         │ "werg"       │ "hotf"       |
| 12        │ "asdf"       │ "qwer"       |
└───────────┴──────────────┴──────────────┘
>>> pt$ getUnion $ ( Union fbbRelX ) `mappend` ( Union fbbRel2 )
<interactive>:73:52:
    Couldn't match type ‘Foo’ with ‘Bar’
    Expected type: Relation '[Bar, Foo, Baz]
      Actual type: FooBarBaz
    In the first argument of ‘Union’, namely ‘fbbRel2’
    In the second argument of ‘mappend’, namely ‘(Union fbbRel2)’
>>> pt$ getUnion $ ( Union $ relRearrange fbbRelX ) `mappend` ( Union fbbRel2 )
┌───────────┬──────────────┬──────────────┐
│ Foo : Int │ Bar : String │ Baz : String │
╞═══════════╪══════════════╪══════════════╡
| 5         │ "asdf"       │ "qwer"       |
| 7         │ "werg"       │ "hotf"       |
| 12        │ "asdf"       │ "qwer"       |
└───────────┴──────────────┴──────────────┘
-}

newtype Union a = Union { getUnion :: a }  
    deriving (Eq, Ord, Read, Show)  

instance Ord r => Monoid (Union (Set r))
    where
        mempty = Union ( empty :: Set r )
        Union x `mappend` Union y = Union ( x `DS.union` y )


{-|
>>> pt$ getXUnion $ ( XUnion fbbRel ) `mappend` ( XUnion fbbRel2 )
┌───────────┬──────────────┬──────────────┐
│ Foo : Int │ Bar : String │ Baz : String │
╞═══════════╪══════════════╪══════════════╡
│ 5         │ "asdf"       │ "qwer"       │
│ 12        │ "asdf"       │ "qwer"       │
└───────────┴──────────────┴──────────────┘
>>> pt$ getXUnion $ ( XUnion fbbRelX ) `mappend` ( XUnion fbbRel2 )
<interactive>:539:55-61:
    Couldn't match type ‘Foo’ with ‘Bar’
    Expected type: Relation '[Bar, Foo, Baz]
      Actual type: FooBarBaz
    In the first argument of ‘XUnion’, namely ‘fbbRel2’
    In the second argument of ‘mappend’, namely ‘(XUnion fbbRel2)’
>>> pt$ getXUnion $ ( XUnion $ relRearrange fbbRelX ) `mappend` ( XUnion fbbRel2 )
┌───────────┬──────────────┬──────────────┐
│ Foo : Int │ Bar : String │ Baz : String │
╞═══════════╪══════════════╪══════════════╡
│ 5         │ "asdf"       │ "qwer"       │
│ 12        │ "asdf"       │ "qwer"       │
└───────────┴──────────────┴──────────────┘
-}

newtype XUnion a = XUnion { getXUnion :: a }  
    deriving (Eq, Ord, Read, Show)  

instance Ord r => Monoid (XUnion (Set r))
    where
        mempty = XUnion ( empty :: Set r )
        XUnion x `mappend` XUnion y = XUnion $ ( x `DS.union` y ) `DS.difference` ( x `DS.intersection` y )


{-
An ideal solution would be a capability to define new types that confine existing types, supplying a function that "hides" some aspect of them, thus allowing them to represent an abstraction within a type system. I'll use the term "cotype" here to describe this, as one can imagine it as the result of a function over a type.

Consider a function that orders the elements of HList records alphabetically by their labels.

rOrder :: ( OrderedRec a b ) => Record a -> Record b

The intent behind this is to be able to view a HList record while ignoring the order of its elements, describing what types we want to consider equal at some higher level of abstraction. With this in place I imagine an alternative way to declare .

Instead of the type synonym in Database.HaskRel.Relational.Definition:

type RTuple = Record

Consider:

cotype RTuple a = rOrder & Record a

The crux of the matter is to be able to use this in place of the type it is defined upon. Monoid above would be one example, but a simpler example would be Eq. With this in place, then when (==) is used against RTuples the following considerations will be made:
 * If the underlying types are the same, then (==) is applied as normal.
 * If types with rOrder applied are the same, then (==) is applied to the 
 * If the types with rOrder applied are different, then it will fail to typecheck.

Alternatively, one may employ a function that takes two arguments, one of which will be morphed to the other, which in the case of RTuple would hRearrange one value to the other. This may be more performant as one will only have to transform one value, but will require a strategy for determining which value of several will be the target value. The rules will then be similar:
 * If the underlying types are the same, then (==) is applied as normal.
 * If ...
 * If ...

A few rules:

t (r a' b') == t (r b' a')

* The result of a transformation upon a type must be a value of the original type, but not neccesarily the same value.

-}
