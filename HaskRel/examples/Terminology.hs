{-# LANGUAGE DataKinds #-}

module Terminology where

import HaskRel.RDBMS
import Data.HList.CommonMain ( Tagged, Label (Label) )

type Haskell = Tagged "haskell" String
type RelModel = Tagged "relModel" String
type SQL = Tagged "sql" String
type HaskRel = Tagged "haskRel" String

haskell = Label::Label "haskell"
relModel = Label::Label "relModel"
sql = Label::Label "sql"
haskRel = Label::Label "haskRel"

type Terms = Relation '[Haskell, RelModel, HaskRel, SQL]

{-| Terms as they appear in Haskell, relational theory, HaskRel and SQL. Both
terms defined in the various systems of computations and terms used in writings
to describe it are included ("function" et. al. being an example of the latter),
the the former capitalized and the latter in lower case. Haskell terms are given
qualified when standing alone, and unqualified when used together. The RelModel
attribute shows the terms as understood from SQL and the Relational Model, 2nd
ed.

Additionally, the when referring to the type itself of "Record a" and "Set (
Record a )" the term "heading" is used in relational theory, and "row type" or
"composite type" in SQL.
-}
terms = relation' [
  ("Data.Tagged.Tagged", "attribute", "Attr", "field, column"),
  ("Data.HList.Record.Record", "tuple", "RTuple", "row"),
  ("Set (Record a)", "relation", "Relation a", "table"),
  ("FilePath (Set (Record a))", "relvar", "Relvar a", "table"),
  ("Data.HList.Record.Label", "attribute name", "Label", "field name, column name"),
  ("Data.HList.Record.Labels", "attribute name set", "Labels", "list of field/column names"),
  ("function, operator", "operator", "function", "operator, function, procedure, routine, method")]
        :: Terms

{- Other ideas:
  ("let", "≝", "let", "N/A"),
  ("let a in b", "WITH a : b ", "let a in b", "WITH a b"),
  (".=.", ":= (attribute)", ".=.", "="),
  ("N/A", ":= (relvar)", "assign, ≔", "N/A")
-}
