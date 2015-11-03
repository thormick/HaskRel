HaskRel, Haskell as a DBMS with support for the relational algebra
==================================================================

(C) 2015, Thor Michael Støre

HaskRel aims to define those elements of the relational theory of database management that Haskell can accommodate, thus enabling Haskell (or more precisely GHC) in its own right as a DBMS with first-class support for those parts of the relational model. It does not qualify as a proper RDBMS since it as-is only defines the relational algebra, relational variables and relational assignment. It does not define the relational calculus, views, constraints and transactions (beyond the fundamental requirement that the tuples of relations are to be unique), certain operators like relation valued aggregate operators, nor a few minor or even deprecated operators such as DIVIDE. The implemented parts are decently complete even if there are major implementation shortcomings that prevent this from being practically usable as an actual DBMS.

I refer to it as _first-class_ since the types of the relational model are first-class types to Haskell, and the Haskell type system is able to induce the type resulting of relational expressions (for instance that a natural join of two relations results in a relation with a heading that is the setwise union of the headings of the original relations).

As such it isn't as much just an implementation of the relational model built using Haskell (the way, say, PostgreSQL is an implementation of SQL using C), as a Haskell library that accommodates a subset of the relational model (unlike PostgreSQL, which doesn't turn C into an RDBMS), where GHCi is employed as a database terminal front-end.

HaskRel is not based on SQL, there is no support for anything that it defines beyond what the relational model does, nor plans for that. It is instead inspired by Tutorial D. For a project of this kind it is generally more sensible to follow developed theory as it stands rather than an industry standard. More specifically, HaskRel is intended as an exploration of the possibilities to directly employ a general purpose programming language and its associated REPL as a relational database management system, or even as a bit of an academic exercise, and a great deal of SQL is not relevant towards this end. (SQL defines for instance its own types, while HaskRel is explicitly intended to build upon Haskell's, and SQL's type system is weaker than Haskell's, in that it employs a wide range of coercions between types). Features and aspects of SQL are either irrelevant for the purpose of HaskRel, or overlap or conflict with Haskell idioms that are more relevant to adhere to for a "make an RDBMS out of a general purpose programming language" project. (Parts of Tutorial D also overlap, such as the `THE_` operator and selectors vs. show and read, but all in all a fraction of what SQL does.) Even if there were plans to go beyond this and support SQL it would be a good first step to first support the fundamentals of the relational model both as _completely_ as it is reasonable to do, and to do so _properly_.

For an understanding of the foundation HaskRel builds on see for instance [SQL and Relational Theory, 2nd ed.](http://shop.oreilly.com/product/0636920022879.do), [The Third Manifesto](http://www.dcs.warwick.ac.uk/~hugh/TTM/TTM-2013-02-07.pdf), and the additional material on [http://www.thethirdmanifesto.com/]().

Of the shortcomings of model that are mentioned above the relational calculus, database constraints, and transactions would be possible to add in a reasonably complete manner (I hope I am able to work on this). Views would be the most difficult, as far as I able to tell it is not possible to properly implement this fully within Haskell, although I don't know if some part of it can be. (This is something SQL products have had difficulties with, after all, and an area where the SQL standard is obtuse.) One further particular issue is that data definition is performed by writing, compiling and loading Haskell, which is particularly static compared to DDL in a proper DBMS.

There are many missing implementation level features that database products rely on, enough so that HaskRel neither is nor will be usable as a production system, even if the shortcomings mentioned regarding the model was in place. Query optimization and indexes are the major ones, just for starters, which would require a monumental effort that there are no plans for. There is also the issue that even slightly complex queries have compile times running into several seconds. HaskRel also builds on Data.Set to manage relation bodies, although this is too limited to support a practically usable RDBMS, at least by itself.

In any case, HaskRel is very, very far from being a usable database management system, and there is as such no plan to bring it there, but I hope it's useful from an academic perspective.


Trying it out
-------------

To get started with an example database cd into examples, run suppliersPartsDB.sh, look through SuppliersPartsExample.hs, and run examples from the Haddock or that file.

Alternatively, if one runs `cabal repl` one will have to either load a file with definitions, or define some elements to play around with. In the example below most everything is ad-hoc, predefining a few values would make certain expressions look less messy. Here are what constants look like:

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

(Note that certain operating systems and browsers have difficulties displaying the double underline of tables. On OS X Firefox has worked better than Chrome.)

As advertised, Haskell infers the type of relational expressions:

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

See "examples/SuppliersPartsDB/Definition.hs" for a way to do this properly.

`assign` against a newly defined relvar initializes it in the file system, and will create a file relative to the directory the interpreter is run from.

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

Concise expression of updates require a set of language extensions:

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

All functions of the relational algebra that HaskRel implement work against both relation constants and relvars, as do the print functions `p` and `pt`:

    *Database.HaskRel.RDBMS> p$ baz `nJoin` bar
    ┌──────┬───────────┬──────┐
    │ asdf │ qwer      │ zxcv │
    ╞══════╪═══════════╪══════╡
    │ 10   │ foo       │ one  │
    │ 30   │ frotz-new │ two  │
    └──────┴───────────┴──────┘


Contributions
-------------

While contributions to HaskRel itself would be great, the limiting factors for HaskRel to develop further, let alone be usable, is in its foundations. The most interesting work that could be done with HaskRel is to go through it and find points where Haskell or its ecosystem could be improved to the benefit of both HaskRel and other projects, and focus development efforts on that. The two most important libraries that HaskRel builds on are HList and Data.Set; efforts towards improving them or arguments towards replacing them would be beneficial.

Making HaskRel usable for practical purposes is not on the horizon, but it may be possible to improve it further towards being a demonstrator of the relational model. A list of "things Haskell needs in order to qualify as a first class RDBMS" would also be a fun list to have, even if there isn't a chance of it happening.
