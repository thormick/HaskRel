{-# LANGUAGE TypeFamilies, FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}

-- TODO: dInsert and iDelete should not just invoke "error", but a proper fix is
-- most likely to define a relational transaction that supports this correctly

{-| 
Module      : Database.HaskRel.Relational.Assignment
Description : Relational assignment
Copyright   : © Thor Michael Støre, 2015
License     : GPL v2 without "any later version" clause
Maintainer  : thormichael át gmail døt com
Stability   : experimental

Relational assignment and specalizations thereof. As with
"Database.HaskRel.Relational.Algebra" this does not support relational
expressions building on relvars, but defers that to
"Database.HaskRel.Relational.Expression".
-}
module Database.HaskRel.Relational.Assignment (
  -- * The primitive assignment function
  assign,
  -- * Specialized assignment functions
  insert, dInsert, update, updateAll, delete, iDelete, deleteP,
  -- * Further specialized and simplified forms of update
  updateA, updateAllA
  ) where

import Control.Monad ( unless )

import Data.HList.CommonMain

import Data.Set ( Set, filter, difference, fromList, size )
import qualified Data.Set ( map, foldr )

import Data.Typeable ( Typeable )

import System.Directory ( renameFile )

import Database.HaskRel.Relational.Definition ( Relation, RTuple, bodyAsList, relRearrange' )
import Database.HaskRel.HFWTabulation ( HPresentRecAttr, showHRecSetTab )

import Database.HaskRel.Relational.Algebra ( intersect, minus, minus_ )
import Database.HaskRel.Relational.Variable


rewriteRelvar
  :: (Show (HList (RecordValuesR r)), RecordValues r) =>
     Relvar a -> Relation r -> IO ()
rewriteRelvar rv updated =
  do writeRelvarBody ( relvarPath rv ++ ".new" ) ( bodyAsList updated )
     renameFile ( relvarPath rv ++ ".new" ) ( relvarPath rv )


-- == Relation variable update operations == --

-- | Writes a relation value to a relvar file, replacing the existing value.
assign
  :: (Ord (HList a), Show (HList (RecordValuesR a)), RecordValues a,
      HRearrange3 (LabelsOf a) r a, HLabelSet (LabelsOf a),
      SameLength' r a, SameLength' r (LabelsOf a), SameLength' a r,
      SameLength' (LabelsOf a) r) =>
     Relvar a -> Relation r -> IO ()
assign rv r = do rewriteRelvar rv ( relRearrange' r $ relvarType rv )
                 putStrLn $ "Value assigned to " ++ relvarPath rv


appendRelvar :: (Show (t a), Foldable t) => Relvar t1 -> t a -> Bool -> IO ()
appendRelvar rv hll empty =
    let prefix = if empty then "" else ","
     in unless (null hll)
            $ appendFile (relvarPath rv) $ prefix ++ init ( tail $ show hll )

-- == Inserts

{-| Inserts a relation into a relvar. This differs from SQL's INSERT; the relvar
is updated to the union of the relvar and the relation value given as arguments.

See `Database.HaskRel.Relational.Expression.insert`.
-}
insert
  :: (Ord (HList a), Read (HList (RecordValuesR a)),
      Show (HList (RecordValuesR a)), RecordValues a,
      HRearrange3 (LabelsOf a) r a, HLabelSet (LabelsOf a),
      HMapAux HList TaggedFn (RecordValuesR a) a, SameLength' r a,
      SameLength' r (LabelsOf a), SameLength' a r,
      SameLength' (LabelsOf a) r) =>
     Relvar a -> Relation r -> IO ()
insert rv r = do
    rv' <- readRelvar rv
    let diff = ( r `minus_` rv' )
        in do appendRelvar rv ( bodyAsList diff ) ( null rv' )
              putStrLn $ "Inserted " ++ show ( size diff ) ++ " of " ++
                         show ( size r ) ++ " tuples into " ++ relvarPath rv
-- Note: "minus_" is used in place of "minus" to rearrange the relation to the relvar, and not vice-versa, as it must be.


{-| Disjoint insert. Closer to SQL INSERT, except that this will never insert a
duplicate tuple.

See `Database.HaskRel.Relational.Expression.dInsert`.
-}
dInsert
  :: (Ord (HList t), Read (HList (RecordValuesR t)),
      Show (HList (RecordValuesR r)), Typeable t, RecordValues r,
      RecordValues t, HRearrange3 (LabelsOf t) r t,
      HLabelSet (LabelsOf t), HMapAux HList TaggedFn (RecordValuesR t) t,
      HFoldr (Mapcar HPresentRecAttr) [[String]] (RecordValuesR t) [[String]],
      SameLength' r t, SameLength' r (LabelsOf t), SameLength' t r,
      SameLength' (LabelsOf t) r) =>
     Relvar t -> Relation r -> IO ()
dInsert rv r = do
    rv' <- readRelvar rv
    let inter = ( rv' `intersect` r )
        in
      if not ( null inter )
         then error $ "Unique constraint violation, tuples already present in " ++ relvarPath rv ++ ":\n" ++ showHRecSetTab inter
         else 
           do appendRelvar rv ( bodyAsList r ) ( null rv' )
              putStrLn $ "Inserted " ++ show ( size r ) ++ " tuples into " ++ relvarPath rv

-- == Updates

-- Warning: Doesn't infer the way I'd like it to.
funSelfUpdate
  :: (HRearrange3 (LabelsOf r') (HAppendListR r r'2) r',
      HLabelSet (LabelsOf r'), HLabelSet (LabelsOf (HAppendListR r r'2)),
      HDeleteLabels (LabelsOf r) r' r'2, HAppendList r r'2,
      SameLength' r' (HAppendListR r r'2),
      SameLength' (LabelsOf r') (HAppendListR r r'2),
      SameLength' (HAppendListR r r'2) r',
      SameLength' (HAppendListR r r'2) (LabelsOf r'),
      HAllTaggedLV (HAppendListR r r'2)) =>
     (Record r' -> Record r) -> Record r' -> Record r'
funSelfUpdate f t = hRearrange ( labelsOf t ) ( f t .<++. t )

update'
  :: (Num t, Num t1, Ord (HList r'),
      HRearrange3 (LabelsOf r') (HAppendListR r r'2) r',
      HLabelSet (LabelsOf r'), HLabelSet (LabelsOf (HAppendListR r r'2)),
      HDeleteLabels (LabelsOf r) r' r'2, HAppendList r r'2,
      SameLength' r' (HAppendListR r r'2),
      SameLength' (LabelsOf r') (HAppendListR r r'2),
      SameLength' (HAppendListR r r'2) r',
      SameLength' (HAppendListR r r'2) (LabelsOf r'),
      HAllTaggedLV (HAppendListR r r'2)) =>
     Set (Record r')
     -> (Record r' -> Bool)
     -> (Record r' -> Record r)
     -> (t, t1, Set (Record r'))
update' r p f = update'' r p (funSelfUpdate f)

updateA'
  :: (Num t, Num t1, Ord (record r), HUpdateAtLabel record l v r r,
      SameLength' r r) =>
     Set (record r) -> (record r -> Bool) -> (record r -> Tagged l v)
     -> (t, t1, Set (record r))
updateA' r p f = update'' r p (\t -> f t .<. t)

update'' :: (Num t, Num t1, Ord a) =>
     Set a -> (a -> Bool) -> (a -> a) -> (t, t1, Set a)
update'' r p f = 
    let (a,b,c) = Data.Set.foldr
                      (\t (a',b',c') ->
                          if p t then ( a' + 1, b' + 1, f t : c' )
                                 else ( a', b' + 1, t : c' ) )
                      (0,0,[])
                      r
        in (a, b, fromList c)

updateAll'
  :: (Num t, Ord (HList r'),
      HRearrange3 (LabelsOf r') (HAppendListR r r'2) r',
      HLabelSet (LabelsOf r'), HLabelSet (LabelsOf (HAppendListR r r'2)),
      HDeleteLabels (LabelsOf r) r' r'2, HAppendList r r'2,
      SameLength' r' (HAppendListR r r'2),
      SameLength' (LabelsOf r') (HAppendListR r r'2),
      SameLength' (HAppendListR r r'2) r',
      SameLength' (HAppendListR r r'2) (LabelsOf r'),
      HAllTaggedLV (HAppendListR r r'2)) =>
     Set (Record r') -> (Record r' -> Record r) -> (t, Set (Record r'))
updateAll' r f = updateAll'' r (funSelfUpdate f)

updateAllA'
  :: (Num t, Ord (record r), HUpdateAtLabel record l v r r,
      SameLength' r r) =>
     Set (record r) -> (record r -> Tagged l v) -> (t, Set (record r))
updateAllA' r f = updateAll'' r (\t -> f t .<. t)

updateAll'' :: (Num t, Ord a1) => Set a -> (a -> a1) -> (t, Set a1)
updateAll'' r f = 
    let (a,b) = Data.Set.foldr (\t (a',b') -> ( a' + 1, f t : b' ) ) (0,[]) r
        in (a, fromList b)


doUpdate
  :: (Show a, Show a1, Show (HList (RecordValuesR r)),
      RecordValues r) =>
     Relvar a2 -> (a, a1, Relation r) -> IO ()
doUpdate rv ( updCount, totCount, updated ) =
  do rewriteRelvar rv updated
     putStrLn $ "Updated " ++ show updCount ++ " of " ++ show totCount ++ " tuples in " ++ relvarPath rv


{-| Updates tuples of a relvar that match the given predicate. As SQL UPDATE.

>>> update sp (\ [pun|pno|] -> pno == "P2" || pno == "P3" ) (\ [pun|qty|] -> case qty - 25 of qty -> [pun|qty|] )
Updated 5 of 12 tuples in SuppliersPartsDB/SP.rv
*SuppliersPartsExample> rPrint$ sp
┌─────┬─────┬─────┐
│ sno │ pno │ qty │
╞═════╪═════╪═════╡
│ S1  │ P1  │ 300 │
│ S1  │ P2  │ 175 │
│ S1  │ P3  │ 375 │
│ S1  │ P4  │ 200 │
...

Note how the cardinality of the relvar will be equal or lower after an update:

>>> assign sp sp'
Value assigned to SuppliersPartsDB/SP.rv
>>> count sp
12
>>> update sp (\[pun|pno|] -> pno == "P1" || pno == "P2" || pno == "P3") (\_ -> _pno "P1" .*. _qty 50 .*. emptyRecord)
Updated 7 of 12 tuples in SuppliersPartsDB/SP.rv
>>> count sp
9
-}
-- TODO: Fix update count message to reflect the situation in the last example
-- above, although this is tricky as this is most likely something that belongs
-- naturally in the set level functions. Note however that it is not feasable to
-- give update counts at all in RDBMSs, as keeping exact track of the
-- cardinality of relvars constitutes an overhead that is in many cases
-- unacceptable, and doesn't provide information that is as useful as a naïve
-- mind might think anyhow.
-- Long term TODO: When constraints are introduced it would be desirable to
-- define a function that requires that the attributes not updated must form a
-- superkey, so that you don't accidentally reduce a relvar when you don't
-- intend to.
update
  :: (Ord (HList a), Read (HList (RecordValuesR a)),
      Show (HList (RecordValuesR a)), RecordValues a,
      HRearrange3 (LabelsOf a) (HAppendListR r r'2) a,
      HLabelSet (LabelsOf a), HLabelSet (LabelsOf (HAppendListR r r'2)),
      HDeleteLabels (LabelsOf r) a r'2,
      HMapAux HList TaggedFn (RecordValuesR a) a, HAppendList r r'2,
      SameLength' a (HAppendListR r r'2),
      SameLength' (LabelsOf a) (HAppendListR r r'2),
      SameLength' (HAppendListR r r'2) a,
      SameLength' (HAppendListR r r'2) (LabelsOf a),
      HAllTaggedLV (HAppendListR r r'2)) =>
     Relvar a -> (Record a -> Bool) -> (Record a -> Record r) -> IO ()
update rv p f = do
    rv' <- readRelvar rv
    doUpdate rv ( update' rv' p f )


{-| Updates all tuples of a relvar. The second argument is a function that results
in an attribute, making for a simpler function than for `update`.

>>> updateA sp (\ [pun|pno|] -> pno == "P2" || pno == "P3" ) (\ [pun|qty|] -> _qty $ qty - 25)
Updated 5 of 12 tuples in SuppliersPartsDB/SP.rv
-}
-- TODO: Can't get the type signature to compile, HUpdateAtLabel2 isn't exported
-- from Data.HList.Record
updateA rv p f = do
    rv' <- readRelvar rv
    doUpdate rv ( updateA' rv' p f )


doUpdateAll
  :: (Show a, Show (HList (RecordValuesR r)), RecordValues r) =>
     Relvar a1 -> (a, Relation r) -> IO ()
doUpdateAll rv ( count, updated ) =
  do rewriteRelvar rv updated
     putStrLn $ "Updated " ++ show count ++ " tuples in " ++ relvarPath rv

{-| Updates tuples of a relvar that match the given predicate.

In SQL and Tutorial D both the predicate of @UPDATE@ is an optional clause, but
optional clauses isn't idiomatic Haskell, hence this separate updateAll
function.

>>> updateAll sp (\ [pun|qty pno|] -> _qty ( qty - 25 ) .*. _pno ( pno ++ "X" ) .*. emptyRecord)
Updated 12 tuples in SuppliersPartsDB/SP.rv
*SuppliersPartsExample> pt sp
┌───────────────┬───────────────┬────────────────┐
│ sno :: String │ pno :: String │ qty :: Integer │
╞═══════════════╪═══════════════╪════════════════╡
│ S1            │ P1X           │ 275            │
...
-}
updateAll
  :: (Ord (HList a), Read (HList (RecordValuesR a)),
      Show (HList (RecordValuesR a)), RecordValues a,
      HRearrange3 (LabelsOf a) (HAppendListR r r'2) a,
      HLabelSet (LabelsOf a), HLabelSet (LabelsOf (HAppendListR r r'2)),
      HDeleteLabels (LabelsOf r) a r'2,
      HMapAux HList TaggedFn (RecordValuesR a) a, HAppendList r r'2,
      SameLength' a (HAppendListR r r'2),
      SameLength' (LabelsOf a) (HAppendListR r r'2),
      SameLength' (HAppendListR r r'2) a,
      SameLength' (HAppendListR r r'2) (LabelsOf a),
      HAllTaggedLV (HAppendListR r r'2)) =>
     Relvar a -> (Record a -> Record r) -> IO ()
updateAll rv f = do
    rv' <- readRelvar rv
    doUpdateAll rv (updateAll' rv' f)

{-| Updates all tuples of a relvar. The second argument is a function that results
in an attribute, making for a simpler function than for `updateAll`.

>>> updateAllA sp (\ [pun|qty|] -> _qty $ qty - 50)
Updated 12 tuples in SuppliersPartsDB/SP.rv
>>> rPrint$ sp
┌───────────────┬───────────────┬────────────────┐
│ sno :: String │ pno :: String │ qty :: Integer │
╞═══════════════╪═══════════════╪════════════════╡
│ S1            │ P1            │ 250            │
...
-}
-- TODO: Can't get the type signature to compile, HUpdateAtLabel2 isn't visible
-- from Data.HList.Record
updateAllA rv f = do
    rv' <- readRelvar rv
    doUpdateAll rv (updateAllA' rv' f)

-- == Deletes

doDelete rv filtered nDeleted =
  do writeRelvarBody ( relvarPath rv ++ ".new" ) ( bodyAsList filtered )
     renameFile ( relvarPath rv ++ ".new" ) ( relvarPath rv )
     putStrLn $ "Deleted " ++ nDeleted ++ " tuples from " ++ relvarPath rv

{-| Deletes a specified subset of a relvar. Note that this is not SQL DELETE, but
instead a generalization thereof.

See `Database.HaskRel.Relational.Expression.delete`.
-}
delete
  :: (Ord (HList t), Read (HList (RecordValuesR t)),
      Show (HList (RecordValuesR t)), RecordValues t,
      HMapAux HList TaggedFn (RecordValuesR t) t) =>
     Relvar t -> Relation t -> IO ()
delete rv r = do
    rv' <- readRelvar rv
    let filtered = Data.Set.difference rv' r
        in doDelete rv filtered ( show $ size rv' - size filtered )

{-| Performs an inclusive delete against a relvar. Also not SQL DELETE. This will
fail if the second argument is not a subset of the relation value identified by
the relation variable reference.

See `Database.HaskRel.Relational.Expression.iDelete`.
-}
iDelete
  :: (Ord (HList a), Ord (HList t), Read (HList (RecordValuesR t)),
      Show (HList (RecordValuesR t)), Typeable a, RecordValues a,
      RecordValues t, HRearrange3 (LabelsOf t) a t,
      HRearrange3 (LabelsOf a) t a, HLabelSet (LabelsOf t),
      HLabelSet (LabelsOf a), HMapAux HList TaggedFn (RecordValuesR t) t,
      HFoldr (Mapcar HPresentRecAttr) [[String]] (RecordValuesR a) [[String]],
      SameLength' a t, SameLength' a (LabelsOf t), SameLength' t a,
      SameLength' t (LabelsOf a), SameLength' (LabelsOf t) a,
      SameLength' (LabelsOf a) t) =>
     Relvar t -> Relation a -> IO ()
iDelete rv r = do
    rv' <- readRelvar rv
    let filtered = rv' `minus` r
        in if size filtered > ( size rv' - size r )
           then error $ "Tuples not found in relvar " ++ relvarPath rv ++ ":\n" ++ showHRecSetTab ( r `minus` rv' )
           else doDelete rv filtered ( show $ size rv' - size filtered )

{- | Delete by predicate, as SQL DELETE.

>>> let newProd = relation [rTuple (pno .=. "P7", pName .=. "Baloon", color .=. "Red", weight .=. (-5 :: Rational), city .=. "Berlin")]
>>> insert p newProd
Inserted 1 of 1 tuples into SuppliersPartsDB/P.rv
>>> deleteP p (\ [pun|pno|] -> pno == "P7" )
Deleted 1 tuples from SuppliersPartsDB/P.rv
-}
deleteP
  :: (Ord (HList t), Read (HList (RecordValuesR t)),
      Show (HList (RecordValuesR t)), RecordValues t,
      HMapAux HList TaggedFn (RecordValuesR t) t) =>
     Relvar t -> (RTuple t -> Bool) -> IO ()
deleteP rv p = do
    rv' <- readRelvar rv
    let filtered = Data.Set.filter ( not . p ) rv'
        in doDelete rv filtered ( show $ size rv' - size filtered )

-- An iDeleteP function could also be defined, but its utility would be
-- marginal.

