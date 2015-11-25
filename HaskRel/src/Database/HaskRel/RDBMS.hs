
{-|
Module      : Database.HaskRel.RDBMS

Description : A re-export of the modules that form the HList record based
              HaskRel library
Copyright   : © Thor Michael Støre, 2015
License     : GPL v2 without "any later version" clause
Maintainer  : thormichael át gmail døt com
Stability   : experimental

Exports the pertinent parts of HaskRel building on HList records. This gives
most features of the relational algebra, relation variable support, HList
CommonMain, as well as certain non-relational features such as ordering.

/This section below belongs in the package description, but Hackage doesn't/
/format the package description in a satisfactory manner/.

= Examples

The examples in this documentation are based on "the old warhorse" that is the
suppliers-parts database (see [1] for more). This gives a body of relational
expressions with known results to base examples upon. See also
examples\/SuppliersPartsExamples.hs (not visible from this documentation) for
Haskell versions of a selection of the Tutorial D expressions given as examples
in chapters 6 and 7 of [1]. These can be run by starting
examples\/suppliersPartsDB.sh and then running @snrt2ndExamples@. While most
Tutorial D expressions translate fairly verbatim to Haskell there are a few
where one must be a bit more explicit. While most Tutorial D expressions
translate fairly verbatim to Haskell there are a few where Haskell is stricter
than Tutorial D and one must be a bit more explicit.

$ is always used after `p`\/`rPrint` or `pt`\/`rPrintTyped` in the examples to
keep them uniform (and so it kinda looks like a prompt), even when not
required. The short forms `p` and `pt` are used whenever there isn't a conflict
with other identifiers, whereas for the SuppliersPartsExample, which has a
relvar "@p@", `rPrint` is used instead of `p` for presentation of relational
objects without type information.

= Terminology

Since this builds on both Haskell and relational theory this documentation uses
terms as they have been established in material related to either. Several terms
of Haskell and HList have been redefined in terms of relational theory in this
library, mostly to illustrate how terms and concepts have been mapped from the
latter to the former. (I'm trying to keep this open to change later if it turns
out to be an unhelpful crutch.)

The following table gives a quick overview of either terms or concepts as found
in Haskell, the relational model (as presented in [1]), HaskRel and SQL, and how
they are mapped from the second to the first:

@
┌───────────────────────────┬────────────────────┬────────────┬────────────────────────────────────────────────┐
│ haskell                   │ relModel           │ haskRel    │ sql                                            │
╞═══════════════════════════╪════════════════════╪════════════╪════════════════════════════════════════════════╡
│ <https://hackage.haskell.org/package/tagged/docs/Data-Tagged.html Data.Tagged.Tagged>        │ attribute          │ Attr       │ field, column                                  │
│ <https://hackage.haskell.org/package/HList/docs/Data-HList-Record.html Data.HList.Record.Record>  │ tuple              │ RTuple     │ row                                            │
│ ( <https://hackage.haskell.org/package/containers/docs/Data-Set.html Set> (<https://hackage.haskell.org/package/HList/docs/Data-HList-Record.html Record> a) )        │ relation           │ Relation a │ table                                          │
│ <https://hackage.haskell.org/package/filepath/docs/System-FilePath.html FilePath> (<https://hackage.haskell.org/package/containers/docs/Data-Set.html Set> (<https://hackage.haskell.org/package/HList/docs/Data-HList-Record.html Record> a)) │ relvar             │ Relvar a   │ table                                          │
│ <https://hackage.haskell.org/package/HList/docs/Data-HList-FakePrelude.html#t:Label Data.HList.Record.Label>   │ attribute name     │ Label      │ field name, column name                        │
│ <https://hackage.haskell.org/package/HList/docs/Data-HList-Record.html#t:Labels Data.HList.Record.Labels>  │ attribute name set │ Labels     │ list of field/column names                     │
│ function, operator        │ operator           │ function   │ operator, function, procedure, routine, method │
└───────────────────────────┴────────────────────┴────────────┴────────────────────────────────────────────────┘
@

Found in "example\/Terminology.hs". Note that this is just an overview, study of
[1] or [2], Haskell itself, HList and HaskRel is required to see how terms and
concepts correlate.

The term \"RTuple\", or "r-tuple", is chosen to simultaneously distinguish the
concept from Haskell tuples while relating it to tuples of the relational
model. For the type of either "Record a" or "Set ( Record a )" in Haskell the
term "heading" is used in relational theory, and "row type" or "composite type"
in SQL. In relational theory the term "scalar" is used to refer to data types
that are neither tuples nor relations, which corresponds to everything but
"Record a" or "Set ( Record a )" in Haskell. Note also that HaskRel /does/ use
the term "table", but then only in the sense of "presentation of a relation
value" (see above).

[1] <http://shop.oreilly.com/product/0636920022879.do SQL and Relational Theory, 2nd ed. (2011), C.J. Date>
[2] <http://www.dcs.warwick.ac.uk/~hugh/TTM/TTM-2013-02-07.pdf The Third Manifesto, C. J. Date and Hugh Darwen, February 7th, 2013>
-}

{- TODO: Should call this something else than RDBMS seeing as it's so far just a
DBMS + relational algebra + base relvars
-}
module Database.HaskRel.RDBMS (
  module Data.HList.CommonMain,
  -- * The relational model of database management: A subset thereof
  module Database.HaskRel.Relational.Definition,
  Relvar ( Relvar, relvarPath ), relvarType, readRelvar,
  module Database.HaskRel.Relational.Expression,
  module Database.HaskRel.Relational.Unicode,
  -- * Non-relational features
  module Database.HaskRel.Order,
  module Database.HaskRel.Support ) where

{- TODO: Only import relevant parts of HList. Variant, TIPs and TICs aren't
required.
-}
import Data.HList.CommonMain
import Data.Tagged (Tagged)

-- The relational model, and features defined together with it
import Database.HaskRel.Relational.Definition
import Database.HaskRel.Relational.Variable ( Relvar (Relvar), relvarPath, relvarType, readRelvar )
import Database.HaskRel.Relational.Expression
import Database.HaskRel.Relational.Unicode

-- Features not of the relational model
import Database.HaskRel.Order
import Database.HaskRel.Support

