{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Definition
Description : Definition and presentation of relational objects in terms of
              Haskell and HList records
Copyright   : © Thor Michael Støre, 2015
License     : GPL v2 without "any later version" clause
Maintainer  : thormichael át gmail døt com
Stability   : experimental

Definition and presentation of relational objects in terms of Haskell and HList
records, and certain implementation level supporting functions.

Naming convention of the relational model is, to a degree, adopted to clarify
how it is mirrored into Haskell. With these aliases in place the following are
equivalent:

>>> :t fromList [] :: Set (Record '[Tagged "sno" String, Tagged "status" Int])

>>> :t relation [] :: Relation '[Attr "sno" String, Attr "status" Int]
-}

module Database.HaskRel.Relational.Definition (
  -- * Definition
  Attr, RTuple, Relation,
  -- ** Labels
  {-| To be in line with the naming convention of the relational model, the @Label@
type could be referred to as an "attribute name", and the @Labels@ type as
"attribute names", but since this would not clarify the matter significantly
HaskRel re-exports them as-is.

It is important to note how labels may either be pre-defined, or used ad-hoc:

>>> let qty = Label :: Label "qty"
>>> sum $ agg qty sp'
3100
>>> sum $ agg ( Label :: Label "qty" ) sp'
3100
  -}
  Label(Label), Labels,
  -- * Construction
  {-| These functions are based on common HList operations, and given aliases that
are in line with relational theory. As mentioned for each function they only
support 0 to 6-tuples, which is of course a drawback. For r-tuples of other
degrees the basic HList approaches to record construction may be used:

>>> let sno = Label :: Label "sno"
... etc ...
>>> pt$ sno .=. "S1" .*. sName .=. "Smith" .*. status .=. (20::Integer) .*. city .=. "London" .*. pName .=. "Ratchet" .*. color .=. "Mauve" .*. pno .=. "P9" .*. emptyRecord
...
>>> type SNO = Tagged "sno" String
... etc ...
>>> pt$ (hEndR $ hBuild "S1" "Smith" 20 "London" "Ratchet" "Mauve" "P9" :: Record '[SNO,SName,Status,City,PName,Color,PNO])
...

Same result.
  -}
  rTuple, rTuple', relation, relation',
  rHdr, as, nAs,
  -- * Constants
  tableDum, tableDee, empty,
  -- * Support
  unordRecEq, bodyAsList, relRearrange, relRearrange',
  -- * Presentation
  HaskRelTS(HaskRelTS), HListTypeSynonym,
  rPrint, rPrintTyped, p, pt, rShowType, showRelTab
    ) where

import Data.Set ( Set, fromList, foldr )
import qualified Data.Set ( map, empty )

import Data.HList.CommonMain

import Database.HaskRel.HFWTabulation
import Data.Typeable


-- == Definition

-- | A shorthand for "attribute", and a synonym of @Data.Tagged@.
type Attr = Tagged

{-| A "tuple as defined by the relational model", synonym for
@Data.HList.Record@.
-}
type RTuple = Record

-- | A synonym for @Set ( RTuple a )@.
type Relation a = Set ( RTuple a )

{-| A shorthand for "relational header", and a synonym of
@Data.HList.Record.Labels@. The header of either an r-tuple or a relation in
relational theory.
-}
type RHdr a = Labels a

{-
Though to make it more concise I'd love to express it something like:
 :: Relation' '[("sno", String), ("status", Int)]
-}

-- == Construction.

-- Instances that allow hToTuple and hFromTuple for unary records and labels
instance HTuple '[Label a] (Label a) where
    hToTuple (a `HCons` HNil) = a
    hFromTuple a = a `HCons` HNil

instance HTuple '[Tagged a a'] (Tagged a a') where
    hToTuple (a `HCons` HNil) = a
    hFromTuple a = a `HCons` HNil

{-| Constructs an r-tuple from tagged values. Supports only 0 to 6-tuples.

>>> pt$ rTuple (sno .=. "S1", sName .=. "Smith", status .=. (20::Integer), city .=. "London")
┌───────────────┬─────────────────┬───────────────────┬────────────────┐
│ sno :: String │ sName :: String │ status :: Integer │ city :: String │
├───────────────┼─────────────────┼───────────────────┼────────────────┤
│ S1            │ Smith           │ 20                │ London         │
└───────────────┴─────────────────┴───────────────────┴────────────────┘
-}
rTuple :: (HLabelSet (LabelsOf r), HTuple r t, HAllTaggedLV r) =>
     t -> RTuple r
rTuple t = mkRecord $ hFromTuple t

{-| Constructs an r-tuple from a tuples of untagged values, where the labels and
exact types are inferred from the context. Supports only 0 to 6-tuples.

>>> pt$ ( rTuple' ("S1", "Smith", 20, "London") :: RTuple '[SNO, SName, Status, City] )
...

Result as for 'rTuple'.
-}
rTuple'
  :: (RecordValues b, HTuple (RecordValuesR b) t,
      HMapAux HList TaggedFn (RecordValuesR b) b) =>
     t -> RTuple b
rTuple' t = hMapTaggedFn $ hFromTuple t

{-| Construct a relation value from a list of r-tuples of tagged values. Alias of
'Data.Set.fromList' with a result restricted to 'Relation', with a name from
relational theory. Supports only 0 to 6-tuples.

>>> :{
pt$ relation [rTuple (sno .=. "S1", sName .=. "Smith", status .=. (20::Integer), city .=. "London"),
              rTuple (sno .=. "S2", sName .=. "Jones", status .=. (10::Integer), city .=. "Paris")]
:}
┌───────────────┬─────────────────┬───────────────────┬────────────────┐
│ sno :: String │ sName :: String │ status :: Integer │ city :: String │
╞═══════════════╪═════════════════╪═══════════════════╪════════════════╡
│ S1            │ Smith           │ 20                │ London         │
│ S2            │ Jones           │ 10                │ Paris          │
└───────────────┴─────────────────┴───────────────────┴────────────────┘
-}
relation :: (Ord a, a ~ Record b) => [a] -> Relation b
relation = fromList

{-| Construct a relation value from a list of tuples of untagged values, where the
labels and exact types are inferred from the context. Supports only 0 to
6-tuples.

>>> pt$ ( relation' [("S1", "Smith", 20, "London"), ("S2", "Jones", 10, "Paris")] :: Relation '[SNO, SName, Status, City] )

Result as for 'relation'.
-}
relation'
  :: (Ord (HList b), RecordValues b, HTuple (RecordValuesR b) t,
      HMapAux HList TaggedFn (RecordValuesR b) b) =>
     [t] -> Relation b
relation' = fromList . map rTuple'

relation''
  :: (Ord (HList b), HLabelSet (LabelsOf b), HTuple b t,
      HAllTaggedLV b) =>
     [t] -> Relation b
relation'' = fromList . map rTuple


{-| Alias of @Data.HList.HList.hFromTuple@ with a name that is more descriptive
for the purpose of constructing "Labels", which are employed as relational
headings, from Haskell tuples of label values. When labels have been defined it
permits expressing:

>>> pt$ p `project` (undefined :: Labels '["pno","pName","color"])

As:

>>> pt$ p `project` (rHdr (pno,pName,color))

Supports 0 to 6-tuples.
-}
rHdr :: HTuple v t => t -> HList v
rHdr = hFromTuple


-- | Synonym of '.=.', for 'rename' and 'nAs'.
as :: Label l -> v -> Tagged l v
as = (.=.)

{-| N-adic constructor of 'as' statements, using @mkRecord . hFromTuple@. Only
supports 0 to 6-tuples.

>>> let pnu = Label :: Label "pnu"
>>> let colour = Label :: Label "colour"
>>> :t nAs (pno `as` pnu, color `as` colour)
nAs (pno `as` pnu, color `as` colour)
  :: Record '[Tagged "pno" (Label "pnu"), Tagged "color" (Label "colour")]
-}
nAs :: (HLabelSet (LabelsOf r), HTuple r t, HAllTaggedLV r) =>
     t -> Record r
nAs = mkRecord . hFromTuple


-- == Supporting functions

{-| Order-agnostic equality operation, which is neccessary for comparison correct
for r-tuples.
-}
unordRecEq
  :: (Eq (HList l), HRearrange3 (LabelsOf l) r l,
      HLabelSet (LabelsOf l), SameLength' r l,
      SameLength' r (LabelsOf l), SameLength' l r,
      SameLength' (LabelsOf l) r) =>
     RTuple l -> RTuple r -> Bool
unordRecEq l r = l == hRearrange' r


-- This variant is quite slow, though that should be because of type system
-- issues and not runtime performance; replicate 10 of a comparison has much
-- better performance than repeating it in a 10-tuple.
unordRecEq'
  :: (Eq (HList l), HRearrange3 (LabelsOf l) r l,
      HLabelSet (LabelsOf l), HLabelSet (LabelsOf r),
      H2ProjectByLabels (LabelsOf l) t r b, SameLength' r l,
      SameLength' r (LabelsOf l), SameLength' l r,
      SameLength' (LabelsOf l) r, HAllTaggedLV r) =>
     RTuple l -> RTuple t -> Bool
unordRecEq' l r = l == hProjectByLabels' r

-- | Rearrange a set of HList records to context. From the perspective of
-- relational theory this is a presentation function.
relRearrange
  :: (Ord (HList l), HRearrange3 (LabelsOf l) r l,
      HLabelSet (LabelsOf l), SameLength' r l,
      SameLength' r (LabelsOf l), SameLength' l r,
      SameLength' (LabelsOf l) r) =>
     Relation r -> Relation l
relRearrange = Data.Set.map hRearrange'

-- | Rearrange a set of HList records to the order of a set given by an
-- argument. The value of the second argument will be ignored. From the
-- perspective of relational theory this is a presentation function.
relRearrange'
  :: (Ord (HList l), HRearrange3 (LabelsOf l) r l,
      HLabelSet (LabelsOf l), SameLength' r l,
      SameLength' r (LabelsOf l), SameLength' l r,
      SameLength' (LabelsOf l) r) =>
     Relation r -> Relation l -> Relation l
relRearrange' rel ord = Data.Set.map hRearrange' rel

-- That ' is swapped between relRearrange and hRearrange is a work related
-- accident and not the way it must be.

rElementType :: Relation r -> RTuple r
rElementType rel = undefined


{-| Gives the body of a relation as a list. This will untag the values. -}
bodyAsList :: RecordValues r => Relation r -> [HList (RecordValuesR r)]
bodyAsList = Data.Set.foldr (\t b -> recordValues t : b ) []
-- TODO: That doesn't work recursively, which it most likely should.



-- == Constants of relational theory

{-| The nullary relation with an empty body

>>> pt$ tableDum
┌──┐
│  │
╞══╡
└──┘
>>> relation [] == tableDum
True
-}
tableDum :: Relation '[]
tableDum = Data.Set.empty

{-| The nullary relation of cardinality one.

>>> pt$ tableDee
┌──┐
│  │
╞══╡
│  │
└──┘
>>> relation [rTuple ()] == tableDee
True
>>> relation' [()] == tableDee
True
-}
tableDee :: Relation '[]
tableDee = fromList[emptyRecord]


-- Redefined and not just reexported to add a bit of documentation
-- TODO: Check if it's possible to have something that'll work for both tableDum and empty.

{-| The empty set without an explicit type. In a relational context this is a
relation with an empty body and no defined heading.

>>> relation [rTuple (sno .=. "S1", status .=. 5)] == empty
False

Note how the example for `tableDum` won't work for `empty` and vice-versa.
-}
empty :: Set a
empty = Data.Set.empty
-- TODO: Or :: Relation a?



-- == Presentation


-- | HaskRel type synonyms
data HaskRelTS = HaskRelTS

-- | HaskRel type synonyms
instance HListTypeSynonym HaskRelTS where
  hRecTS _ = "RTuple"
  hRecSetTS _ = "Relation"
  hTIPTS _ = "RTuple"
  hTIPSetTS _ = "Relation"


-- | Prints a tabular representation of an r-tuple or relation.
rPrint :: HFWPresent r => r -> IO ()
rPrint = hfwPrint

-- | Prints a tabular representation of an r-tuple or relation, with type
-- information.
rPrintTyped :: HFWPresent r => r -> IO ()
rPrintTyped = hfwPrintTypedTS HaskRelTS

-- | Synonym of `rPrint`.
p :: HFWPresent r => r -> IO ()
p = rPrint

-- | Synonym of `rPrintTyped`.
pt :: HFWPresent r => r -> IO ()
pt = rPrintTyped

{-| Specialization of @Database.HaskRel.HFWTabulation.showHTypeTS@ that employs
HaskRel specific type-synonyms.
-}
rShowType :: TypeRep -> String
rShowType = showHTypeTS HaskRelTS


-- Redefined for prettier signatures.
-- | Shows a tabular representation of a relation.
showRelTab ::
     (Typeable a, RecordValues a,
      HFoldr (Mapcar HPresentRecAttr) [[String]] (RecordValuesR a) [[String]]) =>
     Relation a -> String
showRelTab = showHRecSetTab
