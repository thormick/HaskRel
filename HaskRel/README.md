HaskRel, Haskell as a DBMS with support for the relational algebra
==================================================================

(C) 2015, Thor Michael Støre

HaskRel aims to define those elements of the relational theory of database
management that Haskell can accommodate, thus enabling Haskell (or more
precisely GHC) in its own right as a DBMS with first-class support for those
parts of the relational model. It does not qualify as a proper RDBMS since it
as-is only defines the relational algebra, base relational variables and
relational assignment. It does not define the relational calculus, views,
constraints and transactions (beyond the fundamental requirement that the tuples
of relations are to be unique), certain operators like relation valued aggregate
operators, nor a few minor or even deprecated operators such as DIVIDE. The
implemented parts are decently complete even if there are major implementation
shortcomings that prevent this from being practically usable as an actual DBMS.

I refer to it as _first-class_ since the types of the relational model are
first-class types to Haskell, and the Haskell type system is able to induce the
type resulting of relational expressions (for instance that a natural join of
two relations results in a relation with a heading that is the setwise union of
the headings of the original relations).

As such it isn't as much just an implementation of the relational model built
using Haskell (the way, say, PostgreSQL is an implementation of SQL using C), as
a Haskell library that accommodates a subset of the relational model (unlike
PostgreSQL, which doesn't turn C into an RDBMS), where GHCi is employed as a
database terminal front-end.


Model
-----

HaskRel is based on the relational model of database management, as defined by
Chris Date and Hugh Darwen today. For an understanding of the foundation HaskRel
builds on see for instance
[SQL and Relational Theory, 2nd ed.](http://shop.oreilly.com/product/0636920022879.do),
which has been the principal guide in this endeavor (I have plenty of SQL in my
background so a book on SQL and the relational model has been a good
introduction to the latter). See also
[The Third Manifesto (TTM)](http://www.dcs.warwick.ac.uk/~hugh/TTM/TTM-2013-02-07.pdf),
and the additional material on <http://www.thethirdmanifesto.com/> (this might
be better to start with for those mathematically inclined but not steeped in
SQL). Regarding The Third Manifesto it is important to note that all parts of
TTM that overlap with something preexisting in Haskell or which Haskell cannot
support have been disregarded when implementing HaskRel (it's about determining
where we stand, after all).

HaskRel is not based on SQL, there is no support for anything that it defines
beyond what the relational model does, nor plans for that. Tutorial D has been
the reference when implementing the functions of the relational algebra,
although HaskRel does not satisfy the criteria of D. For a project of this kind
it is sensible to follow developed theory as it stands rather than an industry
standard that deviates from it. More specifically, HaskRel is intended as an
exploration of the possibilities to directly employ a general purpose
programming language and its associated REPL as a relational database management
system, or even as a bit of an academic exercise, and a great deal of SQL is not
relevant towards this end. (SQL defines for instance its own types, while
HaskRel is explicitly intended to build upon Haskell's, and SQL's type system is
weaker than Haskell's, in that it employs a wide range of coercions between
types).

Features and aspects of SQL that go beyond or deviate from relational theory are
by and large irrelevant for the purpose of HaskRel, and may even overlap or
conflict with Haskell idioms that are more relevant to adhere to for a "make an
RDBMS out of a general purpose programming language" project. (Parts of Tutorial
D also overlap, such as the `THE_` operator and selectors vs. show and read, but
all in all a fraction of what SQL does.) Even if there were plans to go beyond
this and support SQL it would be a good first step to first support the
fundamentals of the relational model both as _completely_ as it is reasonable to
do, and to do so _properly_.

In addition to naming certain other aspects of Tutorial D has been adopted, such
as argument order. Such aspects may be changed in the future if HaskRel is found
to be solid enough to stand on its own, particularly if there are ways to
express this that are more idiomatic vis-a-vis Haskell (Tutorial D is, after
all, not a definition of the relational model, but a vehicle for understanding
it).

Of the shortcomings of model that are mentioned above the relational calculus,
database constraints, and transactions would be possible to add in a reasonably
complete manner (I hope I am able to work on this). Views would be the most
difficult, as far as I able to tell it is not possible to properly implement
this fully within Haskell, although I don't know if some part of it can
be. (This is something SQL products have had difficulties with, after all, and
an area where the SQL standard is obtuse.) One further particular issue is that
data definition is performed by writing, compiling and loading Haskell, which is
very static compared to DDL in a proper DBMS.


Implementation
--------------

HaskRel is developed and tested with GHC 7.10.2. It builds upon HList (at least
version 0.4.0.0). Separately from this library there is also an optional variant
that builds on TIPs instead of records, which additionally relies on Lens
(tested with version 4.12.3).

There are many missing implementation level features that database products rely
on, enough so that HaskRel neither is nor will be usable as a production system,
even if the shortcomings mentioned regarding the model was in place. Query
optimization and indexes are the major ones, just for starters, which would
require a monumental effort that there are no plans for. There is also the issue
that even slightly complex queries have compile times running into several
seconds. HaskRel also builds on Data.Set to manage relation bodies, although
this is too limited to support a practically usable RDBMS, at least by itself.

In any case, HaskRel is very, very far from being a usable database management
system, and there is as such no plan to bring it there, but I hope it's useful
from an academic perspective.


Trying it out
=============

A Haskell version of the ubiquitous suppliers-and-parts database is included as
a [sample database](examples/SuppliersPartsDB/), with GHC installed this can be
loaded by running suppliersPartsDB.sh in examples from the command line. This
loads [`SuppliersPartsExample.hs`](examples/SuppliersPartsExample.hs), which
contains Haskell renditions of Tutorial D expressions used as examples in SQL
and Relational Theory, 2nd ed chapters 6 and 7. In the documentation see
particularly
[`Database.HaskRel.Relational.Expression`](http://hackage.haskell.org/package/HaskRel/docs/Database-HaskRel-Relational-Expression.html),
which "pulls together" both the algebra and assignment functions and generalizes
them to operate as they should in a DBMS (with caveats), on both base variables
and values.

Alternatively, `cabal repl` will start a GHCi session with the required modules
loaded, although one will have to either load a file with definitions, or define
some elements to play around with. In the example below most everything is
ad-hoc, predefining a few values would make certain expressions look less
messy. Here are what relation constants look like:

    *Database.HaskRel.RDBMS> let foo = relation' [( 10, "foo" ),( 20, "bar" ) ] :: Relation '[Attr "asdf" Int, Attr "qwer" String]
    *Database.HaskRel.RDBMS> let bar = relation' [( 10, "one" ),( 30, "two" ) ] :: Relation '[Attr "asdf" Int, Attr "zxcv" String]
    *Database.HaskRel.RDBMS> pt foo
    ┌─────────────┬────────────────┐
    │ asdf :: Int │ qwer :: String │
    ╞═════════════╪════════════════╡
    │ 10          │ foo            │
    │ 20          │ bar            │
    └─────────────┴────────────────┘
    *Database.HaskRel.RDBMS> pt bar
    ┌─────────────┬────────────────┐
    │ asdf :: Int │ zxcv :: String │
    ╞═════════════╪════════════════╡
    │ 10          │ one            │
    │ 30          │ two            │
    └─────────────┴────────────────┘
    *Database.HaskRel.RDBMS> pt$ foo `naturalJoin` bar
    ┌─────────────┬────────────────┬────────────────┐
    │ asdf :: Int │ qwer :: String │ zxcv :: String │
    ╞═════════════╪════════════════╪════════════════╡
    │ 10          │ foo            │ one            │
    └─────────────┴────────────────┴────────────────┘

(Note that certain operating systems and browsers have difficulties displaying
the horizontal lines of tables, either single or double. On OS X Firefox has
worked better than Chrome.)

As advertised, Haskell infers the type of relational expressions (mind the type
synonyms though):

    *Database.HaskRel.RDBMS> :t foo
    foo :: Relation '[Attr "asdf" Int, Attr "qwer" String]
    *Database.HaskRel.RDBMS> :t bar
    bar :: Relation '[Attr "asdf" Int, Attr "zxcv" String]
    *Database.HaskRel.RDBMS> :t (foo `naturalJoin` bar)
    (foo `nJoin` bar)
      :: containers-0.5.6.2:Data.Set.Base.Set
           (RTuple
              '[Tagged "asdf" Int, Tagged "qwer" String, Tagged "zxcv" [Char]])

Ad-hoc relvars can be defined thusly:

    *Database.HaskRel.RDBMS> let baz = Relvar "baz.rv" :: Relvar '[Attr "asdf" Int, Attr "qwer" String]
    *Database.HaskRel.RDBMS> assign baz (empty :: Relation '[Attr "asdf" Int, Attr "qwer" String] )
    Value assigned to baz.rv

See [Definition.hs](examples/SuppliersPartsDB/Definition.hs) of the
SuppliersPartsDB for a way to do this properly.

`assign` against a newly defined relvar initializes it in the file system, and
will create a file relative to the directory the interpreter is run from.

    *Database.HaskRel.RDBMS> pt baz
    ┌─────────────┬────────────────┐
    │ asdf :: Int │ qwer :: String │
    ╞═════════════╪════════════════╡
    └─────────────┴────────────────┘
    *Database.HaskRel.RDBMS> insert baz foo
    Inserted 2 of 2 tuples into baz.rv
    *Database.HaskRel.RDBMS> pt baz
    ┌─────────────┬────────────────┐
    │ asdf :: Int │ qwer :: String │
    ╞═════════════╪════════════════╡
    │ 10          │ foo            │
    │ 20          │ bar            │
    └─────────────┴────────────────┘
    *Database.HaskRel.RDBMS> insert baz ( relation [rTuple ((Label::Label "asdf") .=. 30, (Label::Label "qwer") .=. "frotz")] )
    Inserted 1 of 1 tuples into baz.rv
    *Database.HaskRel.RDBMS> pt baz
    ┌─────────────┬────────────────┐
    │ asdf :: Int │ qwer :: String │
    ╞═════════════╪════════════════╡
    │ 10          │ foo            │
    │ 20          │ bar            │
    │ 30          │ frotz          │
    └─────────────┴────────────────┘

Concise expression of updates require a set of language extensions (this in addition to DataKinds, which this module enables by default):

    *Database.HaskRel.RDBMS> :set -XQuasiQuotes -XKindSignatures -XViewPatterns
    *Database.HaskRel.RDBMS> update baz (\ [pun|asdf|] -> asdf > 15 ) (\ [pun|qwer|] -> case (qwer ++ "-new") of (qwer) -> [pun|qwer|])
    Updated 2 of 3 tuples in baz.rv
    *Database.HaskRel.RDBMS> pt baz
    ┌─────────────┬────────────────┐
    │ asdf :: Int │ qwer :: String │
    ╞═════════════╪════════════════╡
    │ 10          │ foo            │
    │ 20          │ bar-new        │
    │ 30          │ frotz-new      │
    └─────────────┴────────────────┘

All functions of the relational algebra that HaskRel implement work against both
relation constants and relvars, as do the print functions `p` and `pt`:

    *Database.HaskRel.RDBMS> p$ baz `nJoin` bar
    ┌──────┬───────────┬──────┐
    │ asdf │ qwer      │ zxcv │
    ╞══════╪═══════════╪══════╡
    │ 10   │ foo       │ one  │
    │ 30   │ frotz-new │ two  │
    └──────┴───────────┴──────┘


Contributions
-------------

While contributions to HaskRel itself would be great, the limiting factors for
HaskRel to develop further, let alone be usable, is in its foundations. The most
interesting work that could be done with HaskRel is to go through it and find
points where Haskell or its ecosystem could be improved to the benefit of both
HaskRel and other projects, and focus development efforts on that. The two most
important libraries that HaskRel builds on are HList and Data.Set; efforts
towards improving them or arguments towards replacing them would be beneficial.

Making HaskRel usable for practical purposes is not on the horizon, but it may
be possible to improve it further towards being a demonstrator of the relational
model. A list of "things Haskell/GHC needs in order to qualify as a first class
RDBMS" would also be a fun list to have, even if there isn't a chance of it
happening.
