{-# LANGUAGE ViewPatterns #-} -- View patterns are kind of abused, they're not used to pattern match on a function result, but to just apply a function "between" the lambda expression and its body. It's as such possible to do without them.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module SuppliersPartsExample where

import Database.HaskRelTIP.Relational.Algebra ( relRearrange )
import Database.HaskRelTIP.RDBMSTIP hiding ( p )
import qualified Database.HaskRelTIP.Relational.Algebra as Alg
import Database.HaskRel.Support

import Database.HaskRel.Order

-- Or: :load examples/SuppliersPartsDB/Definition.hs
import SuppliersPartsDB.Definition
import SuppliersPartsDB.Default

import ExampleTypes

-- To make a few type signatures prettier
import Data.Set ( Set )

-- To use HList constructors directly, instead of "relation"/"relation'"
import Data.Set ( fromList )
import Data.HList.CommonMain

{-
λ> s ≔ ( relation' [("S1","Foo", 10, "Bar"),("S2", "zcxv", 20, "poiu")] :: Relation' S )
λ> pt s
┌──────────────┬────────────────┬──────────────┬───────────────┐
│ SNO : String │ SName : String │ Status : Int │ City : String │
╞══════════════╪════════════════╪══════════════╪═══════════════╡
│ "S1"         │ "Foo"          │ 10           │ "Bar"         │
│ "S2"         │ "zcxv"         │ 20           │ "poiu"        │
└──────────────┴────────────────┴──────────────┴───────────────┘
-}


main = do
  recreate
  putStrLn $ "\n"++ show s
  pt$ s
  putStrLn $ "\n"++ show p
  pt$ p
  putStrLn $ "\n"++ show sp
  pt$ sp
  snrt2ndExamples

recreate = do
  putStrLn $ "\nRecreating database at "++ dbPath
  assign s s'
  assign sp sp'
  assign p p'
  putStrLn "\nDone."


r1 = relation [(SNO "S2",PNO "P1"),
               (SNO "S2",PNO "P2"),
               (SNO "S3",PNO "P2"),
               (SNO "S4",PNO "P2"),
               (SNO "S4",PNO "P4"),
               (SNO "S4",PNO "P5")]

r4 = relation [(SNO "S2", PNORel $ uniRelation [(PNO "P1"), (PNO "P2")]),
               (SNO "S3", PNORel $ uniRelation [(PNO "P2")]),
               (SNO "S4", PNORel $ uniRelation [(PNO "P2"),(PNO "P4"),(PNO "P5")])]

-- TODO: This should be a relvar, check that RVAs work for them.
spq = relation [(SNO "S5", SName "Adams", Status 30, City "Athens", PQ $ relation []),
                (SNO "S1", SName "Smith", Status 20, City "London", PQ $ relation [(PNO "P1", QTY 300),(PNO "P2", QTY 200),(PNO "P3", QTY 400),(PNO "P4", QTY 200),(PNO "P5", QTY 100),(PNO "P6", QTY 100)]),
                (SNO "S2", SName "Jones", Status 10, City "Paris", PQ $ relation [(PNO "P1", QTY 300),(PNO "P2", QTY 400)]),
                (SNO "S3", SName "Blake", Status 30, City "Paris", PQ $ relation [(PNO "P2", QTY 200)]),
                (SNO "S4", SName "Clark", Status 20, City "London", PQ $ relation [(PNO "P2", QTY 200),(PNO "P4", QTY 300),(PNO "P5", QTY 400)])]

-- | "undefined" is used so liberally that a concise alias for it is convenient.
-- Alternatives: ü ú ʉ. As a Norwegian I'd go for ø, but I hardly want to impose
-- that on others. In GHC 7.12 it will be possible to redo this with a syntax of
-- "(::TypeGoesHere)". Also note that on OS X (though not in the emacses there)
-- ALT+u gives ü.
ü = undefined

-- | As assumed on 116
pX = p `extend1` (\(Weight weight) -> Status $ ( floor $ weight / 4 ) * 10 )

-- Examples from pp. 108-167 of SQL and Relational Theory 2nd Edition:
snrt2ndExamples = do
  putStrLn "108"
  rPrint$ ( p `nJoin` s ) `restrict2` (\( PName pName, SName sName ) -> pName > sName )
--
  putStrLn "\n110"
  rPrint$ p `restrict1` (\( Weight weight ) -> weight < 17.5 )
--
  putStrLn "\n111"
  rPrint$ p `project` (ü :: RHdr '[Color, City])
--
  putStrLn "\n113"
  rPrint$ ( p `projectAllBut` (ü :: RHdr '[City]) ) `times` ( s `projectAllBut` (ü :: RHdr '[City]) )
--
  putStrLn "\n116"
  rPrint$ ( pX `project` (ü :: RHdr '[Status, City]) ) `union` ( s `project` (ü :: RHdr '[City, Status]) )
  -- See function "shouldFail" below for expression on page 117
--
  putStrLn "\n118"
  rPrint$ ( pX `project` (ü :: RHdr '[Status, City]) ) `intersect` ( s `project` (ü :: RHdr '[City, Status]) )
  rPrint$ ( pX `project` (ü :: RHdr '[Status, City]) ) `minus` ( s `project` (ü :: RHdr '[City, Status]) )
--
  putStrLn "\n119"
  rPrint$
    let r1 = ( rename s (ü :: SNO) (ü :: SA) ) `project` (ü :: RHdr '[SA, SNO])
        r2 = ( rename s (ü :: SNO) (ü :: SB) ) `project` (ü :: RHdr '[SB, SNO])
        r3 = r1 `nJoin` r2
        r4 = r3 `restrict2` (\( SA sa, SB sb ) -> sa < sb )
     in r4 `project` (ü :: RHdr '[SA, SB])
--
  putStrLn "\n122"
  rPrint$ ( rename s (ü :: City) (ü :: SCity)
            `times`
            rename p (ü :: City) (ü :: PCity) )
          `restrict2` (\(SCity sCity, PCity pCity) -> sCity ≠ pCity )
--
  putStrLn "\n123"
  rPrint$ ( s `nJoin` sp ) `restrict1` (\(PNO pno) -> pno == "P2") `projectAllBut` (ü :: RHdr '[PNO] )
  -- TODO: Expressions for the exercises for chapter 6, page 127-130
--
  putStrLn "\n132"
  rPrint$ ( pX `project` (ü :: RHdr '[Status, City]) ) `xUnion` ( s `project` (ü :: RHdr '[City, Status]) )
--
  putStrLn "\n133"
  rPrint$ s `matching` sp
  rPrint$ s `notMatching` sp
  rPrint$ extend1 p (\(Weight weight) -> GMWT $ weight * 454)
--
  putStrLn "\n134"
  rPrint$ ( ( extend1 p
              (\(Weight weight) -> GMWT $ weight * 454) )
            `restrict1` (\(GMWT gmwt) -> gmwt > 7000) )
              `project` (ü :: RHdr '[PNO, GMWT])
--
  putStrLn "\n136"
-- A bit of an issue: relvars are not very useful inside lambdas (would require many instances for IO, including Eq, and similarly for restrict et. al.)
-- Also, I use "!!" to denote the image relation function in this case just to show how that will have to look in Haskell, but since that has to be enclosed in parenthesis I'll instead use "ii" below, which will work as a prefix operator since it starts with an alphabetic character (and kinda looks like !! upside down).
  rPrint$ do spx <- readRelvar sp
             px <- readRelvar p
             s `restrict` (\( image -> (!!) ) ->
                            ( ((!!)spx) `project` (ü :: RHdr '[PNO]) )
                              == px `project` (ü :: RHdr '[PNO]) )
-- The above example uses view patterns to make sure that the expression part of
-- the lambda is specified as in Tutorial D. Alternatively, one could use the
-- tuple directly and employ the "image" function on that:
{-
  rPrint$ do spx <- readRelvar sp
             px <- readRelvar p
             s `restrict` (\t ->
                            ( (t `image` spx) `project` (ü :: RHdr '[PNO]) )
                              == px `project` (ü :: RHdr '[PNO]) )
-}
-- This is necessary when using it against multiple relations of different types,
-- since the former form locks it to a specific argument type.
--
  putStrLn "\n137"
{- Having to specify "uniRelation" instead of "relation" is a huge eyesore, but
that's the way Haskell tuples work (or perhaps, that's the limits to the degree
they work). The alternative is to use HList constructors directly, of the form
"fromList [SNO sno .*. emptyTIP]".
-}
  rPrint$ do spx <- readRelvar sp
             px <- readRelvar p
             s `restrict1` (\(SNO sno) ->
                             ( ( spx `matching` ( uniRelation [(SNO sno)] ) )
                               `project` (ü :: RHdr '[PNO]) )
                             == px `project` (ü :: RHdr '[PNO]) )
  -- Skipping pp. 138-139 an DIVIDE since it's not implemented in HaskRel, see below for the image relation formulation.
--
  putStrLn "\n140"
  -- Just like with lambdas non-relational expressions don't work with direct relvars. Here the result of the operation is bound to a variable...
  do n <- count s
     putStrLn $ show n
  -- ... or readRelvar s could be bound and an expression that takes the relation value can be used:
  do s' <- readRelvar s
     putStrLn $ show $ count (s' `project` (ü :: RHdr '[Status]))
  -- But note: Both "count s" and "count (s `project` (ü :: RHdr '[Status]))" work just fine in GHCi, without any do block (in contrast with lambda expressions).
  putStrLn "1."
  -- Another simiar issue: While "agg"/"aggU" supports types similar to "IO [Integer]", "sum", of course, does not
  do spx <- readRelvar sp
     putStrLn $ show $ sum $ agg' QTY spx
  putStrLn "2."
  do qtys <- aggU ( sp `project` (ü :: RHdr '[QTY]) )
     putStrLn $ show $ sum qtys
  putStrLn "3."
  -- "agg" is a touch more specialized than Tutorial D's "agg op"s, but lists of
  -- values are trivial to manipulate in Haskell:
  do qtys <- agg' QTY sp
     putStrLn $ show $ avg $ map (3*) qtys
--
  putStrLn "\n143"
  do spx <- readRelvar sp
     putStrLn $ show $ sum $ agg' QTY $ spx `restrict1` (\(SNO sno) -> sno == "S5" )
--
  putStrLn "\n144-145"
  putStrLn "1:"
  do spx <- readRelvar sp
     rPrint$ s `restrict` (\( image -> ii ) ->
                            ( sum $ agg' QTY (ii spx) ) < 1000 )
  do spx <- readRelvar sp
     rPrint$ s `restrict1` (\( SNO sno ) ->
                            ( sum $ agg' QTY $ spx `matching` ( uniRelation [(SNO sno)] ) ) < 1000 )
  putStrLn "2:"
  do spx <- readRelvar sp
     rPrint$ s `restrict` (\( image -> ii ) -> ( count (ii spx) ) < 3 )
  putStrLn "3:"
  -- Because QTY is defined to be an integer, Haskell requires a bit more explicit plumbing to multiply it by 0.5 and to compare the result
  do spx <- readRelvar sp
     rPrint$ s `restrict` (\( image -> ii ) ->
                           ( toRational $ minx ( agg' QTY (ii spx) ) 0 )
                             <
                             0.5 * ( toRational $ maxx ( agg' QTY (ii spx) ) 0 ) )
  -- Alternatively:
{-
  do spx <- readRelvar sp
     rPrint$ s `restrict` (\( image -> ii ) ->
                           2 * ( minx ( agg' QTY (ii spx) ) 0 )
                             <
                             ( maxx ( agg' QTY (ii spx) ) 0 ) )
-}
  putStrLn "4:"
  -- See function "updates" below for 5.
  -- TODO: Fix this after a proper rename function has been implemented
  do spx <- readRelvar sp
     rPrint$ sp `restrict` (\( image -> ii ) ->
                            count ( ii $ rename ( rename spx (ü :: SNO) (ü :: SN) ) (ü :: PNO) (ü :: PN) )
                            > 2 )
-- See "updates" function below for example 5
-- TODO: Summarization. Since it's a special case of extension, image relations, and aggregation it's not a major priority
--
  putStrLn "\n152"
  rPrint$ group r1 (ü :: RHdr '[PNO]) PNORel
  rPrint$ r4 `ungroup` (ü :: Label PNORel)
  putStrLn$ show $ ( group r1 (ü :: RHdr '[PNO]) PNORel ) == relRearrange r4
  putStrLn$ show $ r4 `ungroup` (ü :: Label PNORel) == relRearrange r1
--
  putStrLn "\n154 (using pt instead of rPrint here to show the attribute types)"
  pt$ do spx <- readRelvar sp
         extend s (\( image -> ii ) -> PQ (ii spx))
--
  putStrLn "\n155"
  rPrint$ ( spq `ungroup` (ü :: Label PQ) ) `restrict1` (\ (PNO pno) -> pno == "P2" ) `project` (ü :: RHdr '[SNO])
  rPrint$ spq `restrict1` (\ (SNO sno) -> sno == "S2" ) `ungroup` (ü :: Label PQ) `project` (ü :: RHdr '[SNO])
  rPrint$ sp `restrict1` (\ (PNO pno) -> pno == "P2" ) `project` (ü :: RHdr '[SNO])
  rPrint$ sp `restrict1` (\ (SNO sno) -> sno == "S2" ) `project` (ü :: RHdr '[PNO])
--
  -- TODO: putStrLn "\n156"; Insert and update examples in function "updates" below, requires spq as a relvar. See above for the query involving an image relation.
--
  putStrLn "\n157"
  rPrint$
    do spx <- readRelvar sp
       px  <- readRelvar p
       let r1 = extend s  (\( image -> ii ) -> X $ ii spx `project` (ü :: RHdr '[PNO]) )
           r2 = extend r1 (\_               -> Y $ px `project` (ü :: RHdr '[PNO]) )
        in
           r2 `restrict2` (\(X x, Y y) -> x == y)
  -- TODO: Relation valued aggregate operators.
--
  putStrLn "\n158"
  rPrint$
    p `restrict1` (\ ( City city ) -> city == "Paris" )
      `extend1` (\ ( City city ) -> City "Nice" )
      `extend1` (\ ( Weight weight ) -> Weight $ 2 * weight )
  -- Equivalent query that does not rely on extend's behavior of replacing attributes:
  rPrint$
    let r1 = p `restrict1` (\(City city) -> city == "Paris" )
        r2 = r1 `extend` (\_ -> NC "Nice" ) `extend1` (\(Weight weight) -> NW $ weight * 2)
        r3 = r2 `projectAllBut` (ü :: RHdr '[City,Weight] )
     in rename ( rename r3 (ü :: NC) (ü :: City) ) (ü :: NW) (ü :: Weight)
{- Examples on bottom of 158 and top of 159 would be expressed as folows in GHCi:
*SuppliersPartsExample> let s1 = p `restrict1` (\(City city) -> city == "Paris" )
*SuppliersPartsExample> let s2 = ( p `restrict1` (\(City city) -> city == "Paris" ) ) `extend` (\_ -> City "Nice" ) `extend1` (\(Weight weight) -> Weight $ weight * 2)
*SuppliersPartsExample> p `assign` ( p `minus` s1 ) `union` s2

If ≔ is fixed, the last line could be written as:
*SuppliersPartsExample> p ≔ ( p `minus` s1 ) `union` s2
-}
-- Pages 159-163: Recursion. On hold.
--
  putStrLn "\n163"
-- TODO: rtlPrint/rtlPrintTyped (r-tuple-list-print), to give a tabular representation of a tuple list.
  putStrLn $ show $ (s' `matching` sp') `orderOn` (\(hOccurs -> ( SNO sno )) -> Asc sno)
-- TODO: Exercises for chapter 7 on pages 164-167


updates = do
  putStrLn "145"
  putStrLn "5, update of s"
  putStrLn "Pre update"
  rPrint$ s
  do spx <- readRelvar sp
     update s
            (\( image -> ii ) -> ( sum $ agg' QTY (ii spx) ) < 1000 )
            (\( hOccurs -> ( Status status ) ) -> Status $ floor $ 0.5 * toRational status )
  putStrLn "Post update"
  rPrint$ s

shouldFail = do
  putStrLn "117"
  rPrint$ s `project` (ü :: RHdr '[City])
          `dUnion`
          p `project` (ü :: RHdr '[City])


myFun spx ( image -> ii ) = ( sum $ agg' QTY (ii spx) ) < 1000
