{-# LANGUAGE TypeFamilies, FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- TODO! relRearrange where applicable

{- TODO: This doesn't work due to ambigious types:
assign' rv r = assign rv r

All types should be clear from the context. Figure out how to avoid ambiguity. One solution is to add another layer of indirection, creating a "TupleListUpdate" that doesn't have to do bodyAsList, but avoiding ambiguity would be the best solution.

tlAssign :: Show r => Relvar rv -> r -> IO ()
tlAssign rv tl = writeRelvarBody' rv tl

But this would be complicated for functions that perform operations upon the given relation value vis-a-vis the relation variable.
-}

{-| 
Module      : Database.HaskRelTIP.Relational.Assignment
Description : Relational assignment
Copyright   : © Thor Michael Støre, 2015
License     : GPL v2 without "any later version" clause
Maintainer  : thormichael át gmail døt com
Stability   : experimental

Relational assignment and specalizations thereof.
-}
module Database.HaskRelTIP.Relational.Assignment (
  -- * The primitive assignment function
  assign,
  -- * Specialized assignment functions
  insert, dInsert, update, updateAll, delete, iDelete, deleteP ) where

import Control.Monad ( when )

import Data.HList.CommonMain

import Data.Set ( Set, filter, difference, fromList, size )
import qualified Data.Set ( map, foldr )

import Data.Typeable ( Typeable )

import System.Directory ( renameFile )

import Database.HaskRelTIP.Relational.Definition
  ( HWrap, HUnwrap, Relation', HPresentTIP, RDyadic, bodyAsList )
import Database.HaskRelTIP.TIPFWTabulation ( showTIPSetTab )
import Database.HaskRelTIP.Relational.Algebra ( intersect, minus )
import Database.HaskRelTIP.Relational.Variable

-- == Relation variable update operations == --

-- | Writes a relation value to a relvar file, replacing the existing value.
assign :: forall a b ta .
    (Ord (HList b), Show (HList b), TagUntagFD a ta,
     HMapCxt HList HUnwrap a b ) =>
    Relvar ta -> Relation' ta -> IO ()
assign rv r = writeRelvarBody' rv ( bodyAsList r :: [HList b] )


appendRelvar :: (Show (t a), Foldable t) => FilePath -> t a -> Bool -> IO ()
appendRelvar n hll empty =
    let prefix = if empty then "" else ","
     in when ( not $ null hll )
            $ appendFile n ( prefix ++ ( init $ tail $ show $ hll ) )
        

{-|
Inserts a relation into a relvar. This differs from SQLs INSERT; this updates the relvar to the union of the relvar and the relation value given as arguments.
-}
insert :: forall a b ta .
    (Read (HList a), Ord (HList ta), HMapCxt HList HWrap a b,
    -- For minus:
     RDyadic ta ta b b,
    -- For appendRelvar. Note that a and b are reversed vs. appendRelvar
     Show (HList a), Ord (HList a), HMapAux HList HUnwrap b a) =>
    Relvar ta -> Relation' ta -> IO ()
insert rv r = do
    relVarStr <- readFile n
    let relVarVal = ( readHListsToRel ( read ("[" ++ relVarStr ++ "]") :: [HList a] ) :: Relation' ta )
        diff = ( r `minus` relVarVal )
        in do appendRelvar n ( bodyAsList diff :: [HList a] ) False
              putStrLn $ "Inserted " ++ ( show $ size diff ) ++ " of " ++
                         ( show $ size r ) ++ " tuples into " ++ n
     where
         n = relvarPath rv

{-| Disjoint insert. Closer to SQL INSERT, except that this will never insert a
duplicate tuple.

>>> dInsert sp $ relation [(SNO "S6", PNO "P7", QTY 99)]
Inserted 1 tuples into SuppliersPartsDB/SP.rv
>>> dInsert sp $ relation [(SNO "S6", PNO "P7", QTY 99), (SNO "S4", PNO "P4", QTY 300), (SNO "S7", PNO "P8", QTY 200)]
*** Exception: Unique constraint violation, tuples already present in SuppliersPartsDB/SP.rv:
┌─────┬─────┬─────┐
│ SNO │ PNO │ QTY │
╞═════╪═════╪═════╡
│ S4  │ P4  │ 300 │
│ S6  │ P7  │ 99  │
└─────┴─────┴─────┘
-}
dInsert :: forall a b ta .
    (Read (HList a), Ord (HList ta), HMapCxt HList HWrap a b,
    -- For intersect:
     RDyadic ta ta b b,
    -- For showTIPSetTab:
     Typeable b, Typeable ta, HFoldr (Mapcar HPresentTIP) [[String]] b [[String]],
    -- For appendRelvar. Note that a and b are reversed vs appendRelvar
     Show (HList a), Ord (HList a), HMapAux HList HUnwrap b a) =>
    Relvar ta -> Relation' ta -> IO ()
dInsert rv r = do
    relVarStr <- readFile n
    let relVarVal = ( readHListsToRel ( read ("[" ++ relVarStr ++ "]") :: [HList a] ) :: Relation' ta )
        inter = ( relVarVal `intersect` r )
        in
      if not ( null inter )
         then error $ "Unique constraint violation, tuples already present in " ++ n ++ ":\n" ++ ( showTIPSetTab inter )
         else 
           do appendRelvar n ( bodyAsList r :: [HList a] ) ( null relVarVal )
              putStrLn $ "Inserted " ++ ( show $ size r ) ++ " tuples into " ++ n
     where
         n = relvarPath rv


{-| Update. A touch limited today, in SQL you can update as many columns as you'd
like:

@
UPDATE FooBarBaz
   SET foo = foo + 5
       bar = "Foo: " || foo || ", Bar: " || bar || ", Baz: " || baz
 WHERE foo > 6
@

But as-is this only supports updating a single attribute:

>>> update3 "FooBarBaz.rv" (\ (Foo foo, Bar bar, Baz baz) -> foo > 6 ) (\ (Foo foo, Bar bar, Baz baz) -> Bar $ "Foo: " ++ ( show foo ) ++ ", Bar: " ++ bar ++ ", Baz: " ++ baz )

This is completely fixable.


Data.Set.map (\t -> tipyUpdate (Foo 9) t) fbbRel

pt$ extend2 (relation' [( 1, "asdf", "zcxv" ), ( 2, "xzvc", "qewr" )] :: Relation '[Foo,Bar,Baz]) (\( Bar bar, Baz baz ) -> Qux $ baz ++ ", " ++ bar )

extend1 r f = Data.Set.map (\t -> ( f ( hOccurs t ) ) .*. t ) r
extend2 r f = Data.Set.map (\t -> ( f ( tipyTuple t ) ) .*. t ) r
extend3 r f = Data.Set.map (\t -> ( f ( tipyTuple3 t ) ) .*. t ) r
extend4 r f = Data.Set.map (\t -> ( f ( tipyTuple4 t ) ) .*. t ) r
extend5 r f = Data.Set.map (\t -> ( f ( tipyTuple5 t ) ) .*. t ) r
-}

update' ::
     (Ord (record r), HUpdateAtLabel record v v r r, SameLength' r r) =>
     Set (record r) -> (record r -> Bool) -> (record r -> v) -> (Int, Int, Set (record r))
update' r p f =
    let (a,b,c) = Data.Set.foldr
                      (\t (a',b',c') -> if ( p t ) then ( a' + 1, b' + 1, tipyUpdate ( f t ) t : c' )
                                                else ( a', b' + 1, t : c' ) )
                      (0,0,[])
                      r
        in (a, b, fromList c)

{-
update'' ::
     (Ord (record r), HUpdateAtLabel record v v r r,
      SameLength' r r) =>
     Set (record r)
     -> (record r -> Bool) -> (record r -> v) -> Set (record r)
update'' r p f =
    Data.Set.map (\t -> if ( p t ) then tipyUpdate ( f t ) t else t ) r
-}

updateAll' ::
     (Num t, Ord (record r), HUpdateAtLabel record v v r r, SameLength' r r) =>
     Set (record r) -> (record r -> v) -> (t, Set (record r))
updateAll' r f =
    let (a,b) = Data.Set.foldr (\t (a',b') -> ( a' + 1, ( tipyUpdate ( f t ) t ) : b' ) ) (0,[]) r
        in (a, fromList b)
{-
updateAll'' ::
     (Ord (record r), HUpdateAtLabel record v v r r, SameLength' r r) =>
     Set (record r) -> (record r -> v) -> Set (record r)
updateAll'' r f = Data.Set.map (\t -> tipyUpdate ( f t ) t ) r
-}


{-| Updates tuples that match the given predicate. Similar to SQL UPDATE, but whereas SQL UPDATE can update several columns this update function can only update a single attribute. This is not an intensional limit.
-}
update :: forall a b ta v.
    (Read (HList a), Ord (HList ta), TagUntagFD b ta, HMapCxt HList HWrap a b,
    -- For writeRelvarBody/bodyAsList. Note that a and b are reversed vs. appendRelvar
     Show (HList a), Ord (HList a), HMapAux HList HUnwrap b a,
    -- For update'
     HUpdateAtLabel TIP v v ta ta, SameLength' ta ta) =>
    Relvar ta -> ( TIP ta -> Bool ) -> ( TIP ta -> v ) -> IO ()
update rv p f = do
    relVarStr <- readFile n
    let relVarVal = ( readHListsToRel ( read ("[" ++ relVarStr ++ "]") :: [HList a] ) :: Relation' ta )
        ( updCount, notUpdCount, updated ) = update' relVarVal p f
        in do writeRelvarBody ( n ++ ".new" ) ( bodyAsList updated :: [HList a] )
              renameFile ( n ++ ".new" ) n
              putStrLn $ "Updated " ++ ( show updCount ) ++ " of " ++ ( show notUpdCount ) ++ " tuples in " ++ n
     where
         n = relvarPath rv

{-| Updates all tuples of a relvar. In SQL and Tutorial D both the predicate is an optional clause, but optional clauses isn't idiomatic Haskell, hence this separate updateAll function.
-}
updateAll :: forall a b ta v.
    (Read (HList a), Ord (HList ta), TagUntagFD b ta, HMapCxt HList HWrap a b,
    -- For writeRelvarBody/bodyAsList. Note that a and b are reversed vs. appendRelvar
     Show (HList a), Ord (HList a), HMapAux HList HUnwrap b a,
    -- For updateAll'
     HUpdateAtLabel TIP v v ta ta, SameLength' ta ta) =>
    Relvar ta -> ( TIP ta -> v ) -> IO ()
updateAll rv f = do
    relVarStr <- readFile n
    let relVarVal = ( readHListsToRel ( read ("[" ++ relVarStr ++ "]") :: [HList a] ) :: Relation' ta )
        (count :: Int, updated) = updateAll' relVarVal f
        in do writeRelvarBody ( n ++ ".new" ) ( bodyAsList updated :: [HList a] )
              renameFile ( n ++ ".new" ) n
              putStrLn $ "Updated " ++ ( show count ) ++ " tuples in " ++ n
     where
         n = relvarPath rv

{-
*RelExample> :set -XViewPatterns

Update certain tuples:
*RelExample> update "FooBarBaz.rv" (\ ( unTagTIP -> Foo foo `HCons` ( Bar bar `HCons` ( Baz baz `HCons` HNil ) ) ) -> ( foo > 5 ) ) (\ ( unTagTIP -> Foo foo `HCons` ( Bar bar `HCons` ( Baz baz `HCons` HNil ) ) ) -> Bar $ "Foo: " ++ ( show foo ) ++ ", Bar: " ++ bar ++ ", Baz: " ++ baz )
Updated ? tuples in FooBarBaz.rv

Update all tuples:
*RelExample> updateAll "FooBarBaz.rv" (\ ( unTagTIP -> Foo foo `HCons` ( Bar bar `HCons` ( Baz baz `HCons` HNil ) ) ) -> Bar $ "Foo: " ++ ( show foo ) ++ ", Bar: " ++ bar ++ ", Baz: " ++ baz )
Updated ? tuples in FooBarBaz.rv
-}


{-| Deletes a specified subset of a relvar. Note that this is not SQL DELETE, but instead a generalization thereof. -}
delete :: forall a b ta.
    (Read (HList a), Ord (HList ta), TagUntagFD b ta, HMapCxt HList HWrap a b,
    -- For writeRelvarBody/bodyAsList. Note that a and b are reversed vs. appendRelvar
     Show (HList a), Ord (HList a), HMapAux HList HUnwrap b a) =>
    Relvar ta -> Relation' ta -> IO ()
delete rv r = do
    relVarStr <- readFile n
    let relVarVal = ( readHListsToRel ( read ("[" ++ relVarStr ++ "]") :: [HList a] ) :: Relation' ta )
        filtered = Data.Set.difference relVarVal r
        in do writeRelvarBody ( n ++ ".new" ) ( bodyAsList filtered :: [HList a] )
              renameFile ( n ++ ".new" ) n
              putStrLn $ "Deleted " ++ ( show $ size relVarVal - size filtered )  ++ " tuples from " ++ n
     where
         n = relvarPath rv

{-|
Performs an inclusive delete against a relvar. Also not SQL DELETE. This will fail if the second argument is not a subset of the relation value identified by the relation variable reference.
-}
iDelete :: forall a b ta.
    (Read (HList a), Ord (HList ta), HMapCxt HList HWrap a b,
    -- For writeRelvarBody/bodyAsList. Note that a and b are reversed vs. appendRelvar
     Show (HList a), Ord (HList a), HMapAux HList HUnwrap b a,
    -- For minus
     RDyadic ta ta b b,
    -- For showTIPSetTab
     Typeable b, Typeable ta, HFoldr (Mapcar HPresentTIP) [[String]] b [[String]]) =>
    Relvar ta -> Relation' ta -> IO ()
iDelete rv r = do
    relVarStr <- readFile n
    let relVarVal = ( readHListsToRel ( read ("[" ++ relVarStr ++ "]") :: [HList a] ) :: Relation' ta )
        filtered = relVarVal `minus` r
        in if ( size filtered > ( size relVarVal - size r ) )
           then error $ "Tuples not found in relvar " ++ n ++ ":\n" ++ ( showTIPSetTab ( r `minus` relVarVal ) )
           else do writeRelvarBody ( n ++ ".new" ) ( bodyAsList filtered :: [HList a] )
                   renameFile ( n ++ ".new" ) n
                   putStrLn $ "Deleted " ++ ( show $ size relVarVal - size filtered )  ++ " tuples from " ++ n
     where
         n = relvarPath rv


-- | Delete by predicate, as SQL DELETE.
deleteP :: forall a b ta.
    (Read (HList a), Ord (HList ta), TagUntagFD b ta, HMapCxt HList HWrap a b,
    -- For writeRelvarBody/bodyAsList. Note that a and b are reversed vs. appendRelvar
     Show (HList a), Ord (HList a), HMapAux HList HUnwrap b a) =>
    Relvar ta -> ( TIP ta -> Bool ) -> IO ()
deleteP rv p = do
    relVarStr <- readFile n
    let relVarVal = ( readHListsToRel ( read ("[" ++ relVarStr ++ "]") :: [HList a] ) :: Relation' ta )
        filtered = Data.Set.filter ( not . p ) relVarVal
        in do writeRelvarBody ( n ++ ".new" ) ( bodyAsList filtered :: [HList a] )
              renameFile ( n ++ ".new" ) n
              putStrLn $ "Deleted " ++ ( show $ size relVarVal - size filtered )  ++ " tuples from " ++ n
     where
         n = relvarPath rv

-- An iDeleteP function could also be defined, but its utility would be marginal.

{-
*RelExample> deleteP fooBarBaz (\ ( hOccurs -> Foo foo ) -> foo > 5 )
Deleted 3 tuples from FooBarBaz.rv
... Reverted FooBarBaz.rv
*RelExample> deleteP fooBarBaz (\ ( tipyTuple -> ( Foo foo, Baz baz ) ) -> foo < (-5) && baz == "uhb" )
Deleted 0 tuples from FooBarBaz.rv
*RelExample> delete fooBarBaz ( relation' [(5, "asdf", "qwer"), (99, "basf", "nothere")] :: Relation '[Foo, Bar, Baz])
Deleted 1 tuples from FooBarBaz.rv
... Reverted FooBarBaz.rv
*RelExample> iDelete fooBarBaz ( relation' [(5, "asdf", "qwer"), (99, "basf", "nothere")] :: Relation '[Foo, Bar, Baz])
*** Exception: Tuples not found in relvar FooBarBaz.rv:
┌─────┬────────┬───────────┐
│ Foo │ Bar    │ Baz       │
╞═════╪════════╪═══════════╡
│ 99  │ "basf" │ "nothere" │
└─────┴────────┴───────────┘
-}

{-
-- Update could have alternative tipyTuple forms like extend/restrict does
update3' r p f = 
    Data.Set.map (\t -> if ( p $ tipyTuple3 t ) then tipyUpdate ( f $ tipyTuple3 t ) t else t ) r

updateAll3' r f = Data.Set.map (\t -> tipyUpdate ( f $ tipyTuple3 t ) t ) r
-}
-- Well this just got brainmeltingly tricky. This compiles, but I can't get it to run...
{-
update3 :: forall a b ta n v a0 b0 c0
                  v1 v2 v3 v4 v5 v6 v7 v8 v9 t1 t2 t3 t4 t5 t6 t7 t8 t9 v' v'1 v'2.
    (Read (HList a), Ord (HList ta), TagUntagFD b ta, HMapCxt HList HWrap a b,
    -- For writeRelvarBody/bodyAsList. Note that a and b are reversed vs. appendRelvar
     Show (HList a), Ord (HList a), HMapAux HList HUnwrap b a,
    -- For update3': ???????
     HFind1 v (UnLabel v (LabelsOf ta)) n,
     HUpdateAtHNat n (Tagged v v) ta,
     HUpdateAtHNatR n (Tagged v v) ta ~ ta,
     HAllTaggedEq v1, HAllTaggedEq v2,
     HAllTaggedEq v', HAllTaggedEq v3, HAllTaggedEq v4,
     HAllTaggedEq v'1, HAllTaggedEq v5, HAllTaggedEq v6,
     HAllTaggedEq v'2,
     HAllTaggedEq ta, HasField v (Record ta) v,
     HRLabelSet v1, HRLabelSet v2, HRLabelSet v', HRLabelSet v3,
     HRLabelSet v4, HRLabelSet v'1, HRLabelSet v5, HRLabelSet v6,
     HRLabelSet v'2,
     HRLabelSet ta,
     H2ProjectByLabels '[Label a0] v3 t1 v4,
     H2ProjectByLabels '[Label a0] v6 t2 v'2,
     H2ProjectByLabels '[Label a0] ta t3 v1,
     H2ProjectByLabels '[Label b0] v1 t4 v2,
     H2ProjectByLabels '[Label b0] v4 t5 v'1,
     H2ProjectByLabels '[Label b0] ta t6 v5,
     H2ProjectByLabels '[Label c0] v2 t7 v',
     H2ProjectByLabels '[Label c0] v5 t8 v6,
     H2ProjectByLabels '[Label c0] ta t9 v3, HOccurs a0 (TIP v3),
     HOccurs a0 (TIP v6), HOccurs a0 (TIP ta), HOccurs b0 (TIP v1),
     HOccurs b0 (TIP v4), HOccurs b0 (TIP ta), HOccurs c0 (TIP v2),
     HOccurs c0 (TIP v5), HOccurs c0 (TIP ta),
    -- For the type annotations of the result of update3'
     SameLength' ta ta
    ) =>
    FilePath -> ( (a0, b0, c0) -> Bool ) -> ( (a0, b0, c0) -> v ) -> IO ()
update3 n p f = do
    relVarStr <- readFile n
    let relVarVal = ( readHListsToRel ( read ("[" ++ relVarStr ++ "]") :: [HList a] ) :: Relation' ta )
        updated = ( update3' relVarVal p f ) -- :: Relation' ta )
        in do writeRelvarBody ( n ++ ".new" ) ( bodyAsList updated :: [HList a] )
              renameFile ( n ++ ".new" ) n
              putStrLn $ "Updated ? tuples in " ++ n
-}
{- Inferred without type annotations:
      update3 :: forall (a :: [*])
                        (v1 :: [*])
                        (v2 :: [*])
                        (v' :: [*])
                        a1
                        b
                        c
                        (v3 :: [*])
                        (v4 :: [*])
                        (v'1 :: [*])
                        (v5 :: [*])
                        (v6 :: [*])
                        (v'2 :: [*])
                        v
                        (v7 :: [*])
                        (v8 :: [*])
                        (v'3 :: [*])
                        a2
                        b1
                        c1
                        (v9 :: [*])
                        (v10 :: [*])
                        (v'4 :: [*])
                        (v11 :: [*])
                        (v12 :: [*])
                        (v'5 :: [*])
                        (ta :: [*])
                        (b2 :: [*])
                        (a3 :: [*])
                        (n :: HNat)
                        (t1 :: [*])
                        (t2 :: [*])
                        (t3 :: [*])
                        (t4 :: [*])
                        (t5 :: [*])
                        (t6 :: [*])
                        (t7 :: [*])
                        (t8 :: [*])
                        (t9 :: [*])
                        (t10 :: [*])
                        (t11 :: [*])
                        (t12 :: [*])
                        (t13 :: [*])
                        (t14 :: [*])
                        (t15 :: [*])
                        (t16 :: [*])
                        (t17 :: [*])
                        (t18 :: [*]).
                 (Ord (HList ta), Ord (HList b2), Read (HList a), Show (HList b2),
                  TagUntagFD a3 ta, HAllTaggedEq v1, HAllTaggedEq v2,
                  HAllTaggedEq v', HAllTaggedEq v3, HAllTaggedEq v4,
                  HAllTaggedEq v'1, HAllTaggedEq v5, HAllTaggedEq v6,
                  HAllTaggedEq v'2, HAllTaggedEq v7, HAllTaggedEq v8,
                  HAllTaggedEq v'3, HAllTaggedEq v9, HAllTaggedEq v10,
                  HAllTaggedEq v'4, HAllTaggedEq v11, HAllTaggedEq v12,
                  HAllTaggedEq v'5, HAllTaggedEq ta, HasField v (Record ta) v,
                  HRLabelSet v1, HRLabelSet v2, HRLabelSet v', HRLabelSet v3,
                  HRLabelSet v4, HRLabelSet v'1, HRLabelSet v5, HRLabelSet v6,
                  HRLabelSet v'2, HRLabelSet v7, HRLabelSet v8, HRLabelSet v'3,
                  HRLabelSet v9, HRLabelSet v10, HRLabelSet v'4, HRLabelSet v11,
                  HRLabelSet v12, HRLabelSet v'5, HRLabelSet ta,
                  H2ProjectByLabels '[Label a1] v3 t1 v4,
                  H2ProjectByLabels '[Label a1] v6 t2 v'2,
                  H2ProjectByLabels '[Label a1] ta t3 v1,
                  H2ProjectByLabels '[Label b] v1 t4 v2,
                  H2ProjectByLabels '[Label b] v4 t5 v'1,
                  H2ProjectByLabels '[Label b] ta t6 v5,
                  H2ProjectByLabels '[Label c] v2 t7 v',
                  H2ProjectByLabels '[Label c] v5 t8 v6,
                  H2ProjectByLabels '[Label c] ta t9 v3,
                  H2ProjectByLabels '[Label a2] v9 t11 v10,
                  H2ProjectByLabels '[Label a2] v12 t12 v'5,
                  H2ProjectByLabels '[Label a2] ta t10 v7,
                  H2ProjectByLabels '[Label b1] v7 t14 v8,
                  H2ProjectByLabels '[Label b1] v10 t15 v'4,
                  H2ProjectByLabels '[Label b1] ta t13 v11,
                  H2ProjectByLabels '[Label c1] v8 t17 v'3,
                  H2ProjectByLabels '[Label c1] v11 t18 v12,
                  H2ProjectByLabels '[Label c1] ta t16 v9, HOccurs a1 (TIP v3),
                  HOccurs a1 (TIP v6), HOccurs a1 (TIP ta), HOccurs b (TIP v1),
                  HOccurs b (TIP v4), HOccurs b (TIP ta), HOccurs c (TIP v2),
                  HOccurs c (TIP v5), HOccurs c (TIP ta), HOccurs a2 (TIP v9),
                  HOccurs a2 (TIP v12), HOccurs a2 (TIP ta), HOccurs b1 (TIP v7),
                  HOccurs b1 (TIP v10), HOccurs b1 (TIP ta), HOccurs c1 (TIP v8),
                  HOccurs c1 (TIP v11), HOccurs c1 (TIP ta), SameLength' a a3,
                  SameLength' ta ta, SameLength' b2 a3, SameLength' a3 a,
                  SameLength' a3 b2, HMapAux HList HUnwrap a3 b2,
                  HMapAux HList HWrap a a3, HFind1 v (UnLabel v (LabelsOf ta)) n,
                  HUpdateAtHNat n (Tagged v v) ta,
                  HUpdateAtHNatR n (Tagged v v) ta ~ ta) =>
                 FilePath -> ((a1, b, c) -> Bool) -> ((a2, b1, c1) -> v) -> IO ()

-}

{-
update3 fooBarBaz (\ (Foo foo, Bar bar, Baz baz) -> False ) (\ (Foo foo, Bar bar, Baz baz) -> Bar $ "Foo: " ++ ( show foo ) ++ ", Bar: " ++ bar ++ ", Baz: " ++ baz )
-}
