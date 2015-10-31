{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures, QuasiQuotes, ViewPatterns #-}

-- TODO: Does this have more performance issues than the TIP based version?
module SuppliersPartsExample where

import Database.HaskRel.Relational.Definition ( Relation, RTuple, Attr, relation, relation', tableDum, tableDee, empty, rTuple, pt, rPrint )

import Database.HaskRel.RDBMS hiding ( p )
import Database.HaskRel.Support
import Database.HaskRel.Order

import SuppliersPartsDB.Definition
import SuppliersPartsDB.Default

-- To make a few type signatures prettier, and to use HList constructors directly, instead of "relation"/"relation'"
import Data.Set ( Set, fromList )

{- Ad-hoc relvars:
>>> let foo = Relvar "foo.rv" (undefined::Relation S)
>>> foo ≔ ( relation' [("S1","Foo", 10, "Bar"),("S2", "zcxv", 20, "poiu")] :: Relation S )
>>> pt foo
┌───────────────┬─────────────────┬───────────────────┬────────────────┐
│ sno :: String │ sName :: String │ status :: Integer │ city :: String │
╞═══════════════╪═════════════════╪═══════════════════╪════════════════╡
│ S1            │ Foo             │ 10                │ Bar            │
│ S2            │ zcxv            │ 20                │ poiu           │
└───────────────┴─────────────────┴───────────────────┴────────────────┘

And there is now a file foo.rv in your working directory.
-}

makeLabels6 ["pnoRel","pq","sn","pn","x","y","nc","nw","qtys"]

recreate = do
  putStrLn $ "\nRecreating database at "++ dbPath
  assign s s'
  assign sp sp'
  assign p p'
  putStrLn "\nDone."


main = do
  recreate
  putStrLn $ "\n"++ show s
  pt$ s
  putStrLn $ "\n"++ show p
  pt$ p
  putStrLn $ "\n"++ show sp
  pt$ sp
  snrt2ndExamples


r1 = relation [rTuple (sno .=. "S2", pno .=. "P1"),
               rTuple (sno .=. "S2", pno .=. "P2"),
               rTuple (sno .=. "S3", pno .=. "P2"),
               rTuple (sno .=. "S4", pno .=. "P2"),
               rTuple (sno .=. "S4", pno .=. "P4"),
               rTuple (sno .=. "S4", pno .=. "P5")]

r4 = relation [rTuple (sno .=. "S2", pnoRel .=. relation [pno .=. "P1" .*. emptyRecord, pno .=. "P2"  .*. emptyRecord]),
               rTuple (sno .=. "S3", pnoRel .=. relation [pno .=. "P2" .*. emptyRecord]),
               rTuple (sno .=. "S4", pnoRel .=. relation [pno .=. "P2" .*. emptyRecord, pno .=. "P4" .*. emptyRecord, pno .=. "P5" .*. emptyRecord])]


-- TODO: This should be a relvar, check that RVAs work for them.
spq = relation [rTuple (sno .=. "S5", sName .=. "Adams", status .=. 30, city .=. "Athens", pq .=. relation []),
                rTuple (sno .=. "S1", sName .=. "Smith", status .=. 20, city .=. "London", pq .=. relation [rTuple (pno .=. "P1", qty .=. 300), rTuple (pno .=. "P2", qty .=. 200), rTuple (pno .=. "P3", qty .=. 400), rTuple (pno .=. "P4", qty .=. 200), rTuple (pno .=. "P5", qty .=. 100), rTuple (pno .=. "P6", qty .=. 100)]),
                rTuple (sno .=. "S2", sName .=. "Jones", status .=. 10, city .=. "Paris", pq .=. relation [rTuple (pno .=. "P1", qty .=. 300), rTuple (pno .=. "P2", qty .=. 400)]),
                rTuple (sno .=. "S3", sName .=. "Blake", status .=. 30, city .=. "Paris", pq .=. relation [rTuple (pno .=. "P2", qty .=. 200)]),
                rTuple (sno .=. "S4", sName .=. "Clark", status .=. 20, city .=. "London", pq .=. relation [rTuple (pno .=. "P2", qty .=. 200), rTuple (pno .=. "P4", qty .=. 300), rTuple (pno .=. "P5", qty .=. 400)])]

-- | "undefined" is used liberally enough that a concise alias for it is convenient.
ü = undefined

-- | As assumed on page 116
pX = p `extendA` (\[pun|weight|] -> status .=. ( floor ( weight / 4 ) * 10 ))

-- | Examples from pp. 108-167 of SQL and Relational Theory 2nd Edition.
snrt2ndExamples = do
  -- Note that there are many redundant brackets here, either to help make certain expressions more akin to the Tutorial D expressions they are based on, or just to make them slightly more explicit.
  putStrLn "108"
  rPrint$ ( p `nJoin` s ) `restrict` (\[pun|pName sName|] -> pName > sName )
--
  putStrLn "\n110"
  rPrint$ p `restrict` (\[pun|weight|] -> weight < 17.5)
--
  putStrLn "\n111"
  rPrint$ p `project` (rHdr (color, city))
--
  putStrLn "\n113"
  rPrint$ ( p `projectAllBut` (rHdr (city))) `times` ( s `projectAllBut` (rHdr (city)) )
--
  putStrLn "\n116"
  rPrint$ ( pX `project` (rHdr (status, city)) ) `union` ( s `project` (rHdr (city, status)) )
  -- See function "shouldFail" below for expression on page 117
--
  putStrLn "\n118"
  rPrint$ ( pX `project` (rHdr (status, city)) ) `intersect` ( s `project` (rHdr (city,status)) )
  rPrint$ ( pX `project` (rHdr (status, city)) ) `minus` ( s `project` (rHdr (city,status)) )
--
  putStrLn "\n119"
  rPrint$
    let r1 = ( renameA s (sno `as` (Label :: Label "sa")) ) `project` (ü :: Labels '["sa", "sno"])
        r2 = ( renameA s (sno `as` (Label :: Label "sb")) ) `project` (ü :: Labels '["sb", "sno"])
        r3 = r1 `nJoin` r2
        r4 = r3 `restrict` (\[pun|sa sb|] -> sa < sb)
     in r4 `project` (ü :: Labels '["sa", "sb"])
--
  putStrLn "\n122"
  rPrint$ ( renameA s (city `as` (Label :: Label "sCity"))
            `times`
            renameA p (city `as` (Label :: Label "pCity")) )
          `restrict` (\[pun|sCity pCity|] -> sCity ≠ pCity)
--
  putStrLn "\n123"
  rPrint$ ( s `nJoin` sp ) `restrict` (\[pun|pno|] -> pno == "P2") `projectAllBut` (rHdr (pno))
  -- TODO: Expressions for the exercises for chapter 6, page 127-130
--
  putStrLn "\n132"
  rPrint$ ( pX `project` (rHdr (status, city)) ) `xUnion` ( s `project` (rHdr (city,status)) )
--
  putStrLn "\n133"
  rPrint$ s `matching` sp
  rPrint$ s `notMatching` sp
  rPrint$ extend p (\[pun|weight|] -> (Label::Label "gmwt") .=. weight * 454 .*. emptyRecord)
--
  putStrLn "\n134"
  rPrint$ ( ( extend p
              (\[pun|weight|] -> (Label::Label "gmwt") .=. weight * 454 .*. emptyRecord))
            `restrict` (\[pun|gmwt|] -> gmwt > 7000) )
              `project` (ü :: Labels '["pno", "gmwt"])
--
  putStrLn "\n136"
-- A bit of an issue: relvars are as of now not usable inside the function that restrict or extend takes, so they have to be manually read with readRelvar
-- Also, I use "!!" to denote the image relation function in this case just to show how that will have to look in Haskell, but since that has to be enclosed in parenthesis I'll instead use "ii" below, which will work as a prefix operator since it starts with an alphabetic character (and kinda looks like !! upside down).
  rPrint$ do spx <- readRelvar sp
             px <- readRelvar p
             s `restrict` (\( image -> (!!) ) ->
                            ( ((!!)spx) `project` (rHdr (pno)) )
                              == ( px `project` (rHdr (pno)) ) )
-- The above example uses view patterns to make sure that the expression part of
-- the lambda is specified as in Tutorial D. Alternatively, one could use the
-- tuple directly and employ the "image" function on that:
{-
  rPrint$ do spx <- readRelvar sp
             px <- readRelvar p
             s `restrict` (\t ->
                            ( (t `image` spx) `project` (rHdr (pno)) )
                              == px `project` (rHdr (pno)) )
-}
-- This is necessary when using it against multiple relations of different types,
-- since the former form locks it to a specific argument type.
--

  putStrLn "\n137"
-- Note how the variable sno overrides the defined label sno, which is worked around by using the constructor _sno instead. Another alternative would be (Label::Label "sno") .=. sno.
  rPrint$ do spx <- readRelvar sp
             px <- readRelvar p
             s `restrict` (\[pun|sno|] ->
                             ( ( spx `matching` ( relation [rTuple (_sno sno)] ) )
                               `project` (rHdr (pno)) )
                             == px `project` (rHdr (pno)))
  -- Skipping pp. 138-139 and DIVIDE since it's not implemented in HaskRel, see below for the image relation formulation.
--
  putStrLn "\n140"
  -- Just like with lambdas non-relational expressions don't work with direct relvars. Here the result of the operation is bound to a variable...
  do n <- count s
     putStrLn $ show n
  -- ... or readRelvar s could be bound and an expression that takes the relation value can be used:
  do s' <- readRelvar s
     putStrLn $ show $ count (s' `project` (ü :: Labels '["status"]))
  -- But note: Both "count s" and "count (s `project` (ü :: Labels '["status"]))" work just fine in GHCi, without any do block or similar (in contrast with lambda expressions).
  putStrLn "1."
  -- Another similar issue: While "agg"/"aggU" supports types similar to "IO [Integer]", "sum", of course, does not
  do spx <- readRelvar sp
     putStrLn $ show $ sum $ agg qty spx
  putStrLn "2."
  do qtys <- aggU ( sp `project` (ü :: Labels '["qty"]) )
     putStrLn $ show $ sum qtys
  putStrLn "3."
  -- "agg" is a touch more specialized than Tutorial D's "agg op"s, but lists of
  -- values are trivial to manipulate in Haskell:
  do qtys <- agg qty sp
     putStrLn $ show $ avg $ map (3*) qtys
--
  putStrLn "\n143"
  do spx <- readRelvar sp
     putStrLn $ show $ sum $ agg qty $ spx `restrict` (\[pun|sno|] -> sno == "S5")
--
  putStrLn "\n144-145"
  putStrLn "1:"
  do spx <- readRelvar sp
     rPrint$ s `restrict` (\( image -> ii ) ->
                            ( sum $ agg qty (ii spx) ) < 1000)
  do spx <- readRelvar sp
     rPrint$ s `restrict` (\[pun|sno|] ->
                            ( sum $ agg qty $ spx `matching` ( relation [rTuple ((Label::Label "sno") .=. sno)] ) ) < 1000)
  putStrLn "2:"
  do spx <- readRelvar sp
     rPrint$ s `restrict` (\( image -> ii ) -> ( count (ii spx) ) < 3)
  putStrLn "3:"
  -- Because QTY is defined to be an integer, Haskell requires a bit more explicit plumbing to multiply it by 0.5 and to compare the result
  do spx <- readRelvar sp
     rPrint$ s `restrict` (\( image -> ii ) ->
                           ( toRational $ minx ( agg qty (ii spx) ) 0 )
                             <
                             0.5 * ( toRational $ maxx ( agg qty (ii spx) ) 0 ))
{-
  -- Alternatively:
  do spx <- readRelvar sp
     rPrint$ s `restrict` (\( image -> ii ) ->
                           2 * ( minx ( agg qty (ii spx) ) 0 )
                             <
                             ( maxx ( agg qty (ii spx) ) 0 ) )
-}
  putStrLn "4:"
  -- See function "updates" below for 5.
  -- TODO: Fix this after rename has been improved
  do spx <- readRelvar sp
     rPrint$ sp `restrict` (\( image -> ii ) ->
                            count ( ii $ renameA (renameA spx (sno `as` sn)) (pno `as` pn) )
                            > 2)
-- See "updates" function below for example 5
-- TODO: Summarization. Since it's a special case of extension, image relations, and aggregation it's not a major priority
--
  putStrLn "\n152"
  rPrint$ group r1 (rHdr (pno)) (pnoRel .=.)
  rPrint$ r4 `ungroup` (ü :: Label "pnoRel")
  putStrLn$ show $ ( group r1 (rHdr (pno)) (pnoRel .=.) ) == relRearrange r4
  putStrLn$ show $ r4 `ungroup` (ü :: Label "pnoRel") == relRearrange r1
--
  putStrLn "\n154 (using pt instead of rPrint here to show the attribute types)"
  pt$ do spx <- readRelvar sp
         extend s (\( image -> ii ) -> pq .=. (ii spx) .*. emptyRecord)
--
  putStrLn "\n155"
  rPrint$ ( spq `ungroup` (ü :: Label "pq") ) `restrict` (\[pun|pno|] -> pno == "P2") `project` (rHdr (sno))
  rPrint$ spq `restrict` (\[pun|sno|] -> sno == "S2") `ungroup` (ü :: Label "pq") `project` (rHdr (sno))
  rPrint$ sp `restrict` (\[pun|pno|] -> pno == "P2") `project` (rHdr (sno))
  rPrint$ sp `restrict` (\[pun|sno|] -> sno == "S2") `project` (rHdr (pno))
--
  -- TODO: putStrLn "\n156"; Insert and update examples in function "updates" below, requires spq as a relvar. See above for the query involving an image relation.
--
  putStrLn "\n157"
  rPrint$
    do spx <- readRelvar sp
       px  <- readRelvar p
       let r1 = extendA s  (\( image -> ii ) -> x .=. (ii spx `project` (rHdr (pno))))
           r2 = extendA r1 (\_               -> y .=. (px `project` (rHdr (pno))))
        in
           r2 `restrict` (\[pun|x y|] -> x == y)
  -- TODO: Relation valued aggregate operators.
--
  putStrLn "\n158"
  rPrint$
    p `restrict` (\[pun|city|] -> city == "Paris")
      `extendA` (\_ -> city .=. "Nice")
      `extendA` (\[pun|weight|] -> _weight $ 2 * weight)
  -- Equivalent query that does not rely on extend's behavior of replacing attributes:
  rPrint$
    let r1 = p `restrict` (\[pun|city|] -> city == "Paris")
        r2 = r1 `extendA` (\_ -> nc .=. "Nice" ) `extendA` (\[pun|weight|] -> nw .=. weight * 2)
        r3 = r2 `projectAllBut` (rHdr (city,weight))
     in ( r3 `rename` nAs ((nc `as` city),(nw `as` weight)) )
{-
The examples on the bottom of page 158 and top of 159 would be expressed as follows in GHCi:

*SuppliersPartsExample> let s1 = p `restrict` (\[pun|city|] -> city == "Paris" )
*SuppliersPartsExample> let s2 = ( p `restrict` (\[pun|city|] -> city == "Paris" ) ) `extendA` (\_ -> city .=. "Nice" ) `extendA` (\[pun|weight|] -> (Label::Label "weight") .=. weight * 2)
*SuppliersPartsExample> p ≔ ( p `minus` s1 ) `union` s2
-}
-- Pages 159-163: Recursion. TODO: On hold.
--
  putStrLn "\n163"
-- TODO: rtlPrint/rtlPrintTyped (r-tuple-list-print), to give a tabular representation of a tuple list.
  putStrLn $ show $ (s' `matching` sp') `orderOn` (\[pun|sno|] -> Asc sno)
-- TODO: Exercises for chapter 7 on pages 164-167


updates = do
  putStrLn "145"
  putStrLn "5, update of s"
  putStrLn "Pre update"
  rPrint$ s
  do spx <- readRelvar sp
     update s
            (\( image -> ii ) -> ( sum $ agg qty (ii spx) ) < 1000)
            (\[pun|status|] -> _status (floor $ 0.5 * toRational status) .*. emptyRecord)
  putStrLn "Post update"
  rPrint$ s

shouldFail = do
  putStrLn "117"
  rPrint$ (s `project` rHdr city)
          `dUnion`
          (p `project` rHdr city)


myFun spx ( image -> ii ) = ( sum $ agg qty (ii spx) ) < 1000

