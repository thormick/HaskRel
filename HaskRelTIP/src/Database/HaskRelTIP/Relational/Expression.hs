{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TypeFamilies, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables, AllowAmbiguousTypes #-}

{-|
Module      : Database.HaskRelTIP.Relational.Expression
Description : Support for relational expressions
Copyright   : © Thor Michael Støre, 2015
License     : GPL v2 without "any later version" clause
Maintainer  : thormichael át gmail døt com
Stability   : experimental

"Database.HaskRelTIP.Relational.Algebra" defines the functions of the relational algebra, but in order to keep pertinent concerns separated it only defines functions for relational operations upon values, not upon relation variables. This module redefines those functions such that they operate upon relation values, relation variables and relational IO (expressions that build upon relvars), and also adds `Relational` instances for relvars and relational IO.

This approach, while providing a language that operates like a proper query language, has an unfortunate issue. The best example for illustrating this found in light of how relational theory calls for relations to be comparable, which as mentioned for `Database.HaskRelTIP.Relational.Definition.unordTIPEq` already has one shortcoming in HaskRel. Bringing relation variables into the picture further complicates this, because then either or both sides of `==` will potentially be IO (Relation a). The solution is to eschew this module, rely on the underlying modules, and manually write a do-block:

>>> do
    r1 <- readRelvar fooBarBaz
    putStrLn $ show $ r1 == fbbRel
False

An alternative would be to define an entire range of IO(Bool) functions.

This approach also has the implication that the type signatures of the functions become very confusing, and it is therefore often more appropriate to look up the type signatures of the identically named functions in Database.HaskRelTIP.Relational.Algebra and Database.HaskRelTIP.Relational.Assignment that they generalize.
-}
module Database.HaskRelTIP.Relational.Expression (
  -- * Functions defined as monadic operators in relational theory.
  {-| (Not to be confused with Haskell monads.) -}
  -- ** The monadic operator class
  MonOp, MonOp', MonOpArg,
  -- ** The functions defined as monadic operators in the relational algebra
  rename, extend, restrict, project, projectAllBut,
  group, groupAllBut, ungroup, count,
  -- ** Assignment functions
  assign,
  -- ** Relevant functions sugared for Haskell
  extend1, extend2, extend3, extend4, extend5,
  restrict1, restrict2, restrict3, restrict4, restrict5,
  -- ** Peripheral functions, not part of relational theory
  rfoldr1, rfoldr2, rfoldr3, rfoldr4, rfoldr5, rafoldr, rafoldr', rafoldrU, agg, agg', aggU,
  image, member, notMember, dExtend,
  -- * Functions defined as dyadic operators in relational theory
  -- ** The dyadic operator class
  DyaOp, DyaOp', DyaOpLeft, DyaOpRight,
  -- ** The functions defined as dyadic operators in the relational algebra
  naturalJoin, nJoin, matching, semiJoin, leftSemiJoin, notMatching,
  leftNotMatching, semiDiff, leftSemiDiff, times, interJoin, iJoin, union, dUnion,
  intersect, minus, xUnion, xMinus, isProperSubsetOf, isSubsetOf
      ) where

import Data.Set (Set, fromList, toList)
import qualified Data.Set (member, notMember)
import Control.Lens (Wrapped, Unwrapped) -- Not required to compile, but pretties the type signatures

import Data.HList.CommonMain

import Data.Data

import Database.HaskRel.HFWTabulation ( HFWPresent, hfwPrint, hfwPrintTypedTS )
import Database.HaskRelTIP.TIPFWTabulation ( printTIPSet, printTIPSetTypedTS )

import Database.HaskRelTIP.Relational.Definition

import qualified Database.HaskRelTIP.Relational.Algebra as Algebra
import Database.HaskRelTIP.Relational.Variable

{-
It would be significantly simpler to only support IO variations of the relational
operators, but I consider direct support for
relation constants -> relation constant
a more complete approach.
-}

-- "HFWPresent" instances for IO and Relvars
instance (Typeable a, Typeable ta, TagUntagFD a ta,
      HFoldr (Mapcar HPresentTIP) [[String]] a [[String]],
      HFoldr (Mapcar HPresentTIPTyped) [[String]] a [[String]],
      Data (HList a)) =>
      HFWPresent ( IO ( Relation' ta ) )
    where
      hfwPrint r' = r' >>= printTIPSet
      hfwPrintTypedTS ts r' = r' >>= ( printTIPSetTypedTS ts )


instance (Data (HList a1), Ord (HList ta), Read (HList a),
     Typeable ta, Typeable a1,
     TagUntagFD a1 ta, HMapCxt HList HWrap a a1,
     HFoldr (Mapcar HPresentTIP) [[String]] a1 [[String]],
     HFoldr (Mapcar HPresentTIPTyped) [[String]] a1 [[String]]) =>
     HFWPresent ( Relvar ta )
    where
      hfwPrint r' = do
          r :: ( Relation' ta ) <- readRelvar' r' (undefined :: HList a)
          printTIPSet r
      hfwPrintTypedTS ts r' = do
          r :: ( Relation' ta ) <- readRelvar' r' (undefined :: HList a)
          ( printTIPSetTypedTS ts r )


class MonOp a where
    type MonOp' a res :: *
    type MonOp' a res = IO res
    type MonOpArg a :: *
    monOp :: (MonOpArg a -> res) -> a -> (MonOp' a res)

instance MonOp (Relation' a) where
    type MonOp' (Relation' a) res = res
    type MonOpArg (Relation' a) = (Relation' a)
    monOp f r = f r

instance MonOp (IO (Relation' a)) where
    type MonOpArg (IO (Relation' a)) = Relation' a
    monOp f r = r >>= return . f

instance (Ord (HList a),
          TagUntagFD b a,
          HMapCxt HList HWrap c b,
          Read (HList c) ) =>
         MonOp (Relvar a) where
    type MonOpArg (Relvar a) = Relation' a
    monOp f r = do
            ( r' :: Relation' a ) <- readRelvar' r ( undefined :: HList c )
            return $ f r'

rename r from to = monOp (\r' -> Algebra.rename r' from to ) r

extend r f = monOp (\r' -> Algebra.extend r' f ) r

extend1 r f = monOp (\r' -> Algebra.extend1 r' f ) r
extend2 r f = monOp (\r' -> Algebra.extend2 r' f ) r
extend3 r f = monOp (\r' -> Algebra.extend3 r' f ) r
extend4 r f = monOp (\r' -> Algebra.extend4 r' f ) r
extend5 r f = monOp (\r' -> Algebra.extend5 r' f ) r

dExtend r f = monOp (\r' -> Algebra.dExtend r' f ) r

restrict r f = monOp (\r' -> Algebra.restrict r' f ) r

restrict1 r f = monOp (\r' -> Algebra.restrict1 r' f ) r
restrict2 r f = monOp (\r' -> Algebra.restrict2 r' f ) r
restrict3 r f = monOp (\r' -> Algebra.restrict3 r' f ) r
restrict4 r f = monOp (\r' -> Algebra.restrict4 r' f ) r
restrict5 r f = monOp (\r' -> Algebra.restrict5 r' f ) r

project r a = monOp (\r' -> Algebra.project r' a ) r

projectAllBut r a = monOp (\r' -> Algebra.projectAllBut r' a ) r

group rel attsIn attOut = monOp (\r' -> Algebra.group r' attsIn attOut ) rel

groupAllBut rel attsIn attOut = monOp (\r' -> Algebra.groupAllBut r' attsIn attOut ) rel

ungroup r a = monOp (\r' -> Algebra.ungroup r' a ) r

count r = monOp Algebra.count r

infix 1 `assign`
--
assign :: forall a a1 b rv ta .
          (Ord (HList b), Show (HList b), TagUntagFD a1 ta, SameLength' b a1,
           SameLength' a1 b, HMapAux HList HUnwrap a1 b, MonOp a,
           MonOpArg a ~ Set (TIP ta)) =>
          Relvar rv -> a -> MonOp' a (IO ())
assign rv r = monOp (\r' -> writeRelvarBody' rv ( bodyAsList r' :: [HList b] ) ) r

-- TODO: Rest of Assignment. But they're problematic...


-- 
rfoldr1 f b r = monOp (\r' -> Algebra.rfoldr1 f b r' ) r
rfoldr2 f b r = monOp (\r' -> Algebra.rfoldr2 f b r' ) r
rfoldr3 f b r = monOp (\r' -> Algebra.rfoldr3 f b r' ) r
rfoldr4 f b r = monOp (\r' -> Algebra.rfoldr4 f b r' ) r
rfoldr5 f b r = monOp (\r' -> Algebra.rfoldr5 f b r' ) r

rafoldr f b a r = monOp (\r' -> Algebra.rafoldr f b a r' ) r

rafoldr' f b a r = monOp (\r' -> Algebra.rafoldr' f b a r' ) r

rafoldrU f b r = monOp (\r' -> Algebra.rafoldrU f b r' ) r

agg a r = monOp (\r' -> Algebra.agg a r' ) r

agg' a r = monOp (\r' -> Algebra.agg' a r' ) r

aggU r = monOp (\r' -> Algebra.aggU r' ) r

-- TODO: Check that this works not just towards an IO(Relation a) but also from it
image t r = monOp (\r' -> Algebra.image t r' ) r

member e r = monOp (\r' -> Data.Set.member e r' ) r

notMember e r = monOp (\r' -> Data.Set.notMember e r' ) r


-- Functions defined as dyadic operators in relational theory
class DyaOp a b where
    type DyaOp' a b res :: *
    type DyaOp' a b res = IO res
    type DyaOpLeft  a :: *
    type DyaOpRight b :: *
    dyaOp :: (DyaOpLeft a -> DyaOpRight b -> res) -> a -> b -> (DyaOp' a b res)

instance DyaOp (Relation' a1) (Relation' a2) where
    type DyaOp' (Relation' a1) (Relation' a2) res = res
    type DyaOpLeft  (Relation' a1) = Relation' a1
    type DyaOpRight (Relation' a2) = Relation' a2
    dyaOp f r1 r2 = f r1 r2

instance DyaOp (IO (Relation' a1)) (Relation' a2) where
    type DyaOpLeft  (IO (Relation' a1)) = Relation' a1
    type DyaOpRight (Relation' a2) = Relation' a2
    dyaOp f r1 r2 = do
        r1' <- r1
        return $ f r1' r2

instance DyaOp (Relation' a1) (IO (Relation' a2)) where
    type DyaOpLeft  (Relation' a1) = Relation' a1
    type DyaOpRight (IO (Relation' a2)) = Relation' a2
    dyaOp f r1 r2 = r2 >>= return . ( f r1 )

instance DyaOp (IO (Relation' a1)) (IO (Relation' a2)) where
    type DyaOpLeft  (IO (Relation' a1)) = Relation' a1
    type DyaOpRight (IO (Relation' a2)) = Relation' a2
    dyaOp f r1 r2 = do
        r1' <- r1
        r2' <- r2
        return $ f r1' r2'
-- r1 >>= (\r1' -> r2 >>= return . ( f r1' ) )
-- !
-- r1 >>= r2 >>= return . f
-- ?

instance (Ord (HList a2), TagUntagFD b2 a2,
          HMapCxt HList HWrap c2 b2,
          Read (HList c2) ) =>
         DyaOp (Relation' a1) (Relvar a2) where
    type DyaOpLeft  (Relation' a1) = Relation' a1
    type DyaOpRight (Relvar a2) = Relation' a2
    dyaOp f r1 r2 = do
        ( r2' :: Relation' a2 ) <- readRelvar' r2 ( undefined :: HList c2 )
        return $ f r1 r2'

instance (Ord (HList a1),
          TagUntagFD b1 a1,
          HMapCxt HList HWrap c1 b1,
          Read (HList c1) ) =>
         DyaOp (Relvar a1) (Relation' a2) where
    type DyaOpLeft  (Relvar a1) = Relation' a1
    type DyaOpRight (Relation' a2) = Relation' a2
    dyaOp f r1 r2 = do
        ( r1' :: Relation' a1 ) <- readRelvar' r1 ( undefined :: HList c1 )
        return $ f r1' r2

instance (Ord (HList a1),
          TagUntagFD b1 a1,
          HMapCxt HList HWrap c1 b1,
          Read (HList c1) ) =>
         DyaOp (Relvar a1) (IO (Relation' a2)) where
    type DyaOpLeft  (Relvar a1) = Relation' a1
    type DyaOpRight (IO (Relation' a2)) = Relation' a2
    dyaOp f r1 r2 = do
        ( r1' :: Relation' a1 ) <- readRelvar' r1 ( undefined :: HList c1 )
        r2' <- r2
        return $ f r1' r2'

instance (Ord (HList a2), TagUntagFD b2 a2,
          HMapCxt HList HWrap c2 b2,
          Read (HList c2) ) =>
         DyaOp (IO (Relation' a1)) (Relvar a2) where
    type DyaOpLeft (IO (Relation' a1)) = Relation' a1
    type DyaOpRight (Relvar a2)     = Relation' a2
    dyaOp f r1 r2 = do
        r1' <- r1
        ( r2' :: Relation' a2 ) <- readRelvar' r2 ( undefined :: HList c2 )
        return $ f r1' r2'

instance (Ord (HList a1), Ord (HList a2), 
          TagUntagFD b1 a1, TagUntagFD b2 a2,
          HMapCxt HList HWrap c1 b1,
          HMapCxt HList HWrap c2 b2,
          Read (HList c1), Read (HList c2) ) =>
         DyaOp (Relvar a1) (Relvar a2) where
    type DyaOpLeft  (Relvar a1) = Relation' a1
    type DyaOpRight (Relvar a2) = Relation' a2
    dyaOp f r1 r2 = do
        ( r1' :: Relation' a1 ) <- readRelvar' r1 ( undefined :: HList c1 )
        ( r2' :: Relation' a2 ) <- readRelvar' r2 ( undefined :: HList c2 )
        return $ f r1' r2'


naturalJoin r1 r2 = dyaOp Algebra.naturalJoin r1 r2
nJoin r1 r2 = dyaOp Algebra.nJoin r1 r2

matching r1 r2 = semiJoin r1 r2

semiJoin r1 r2 = dyaOp Algebra.semiJoin r1 r2
leftSemiJoin r1 r2 = dyaOp Algebra.semiJoin r2 r1

notMatching r1 r2 = semiDiff r1 r2
leftNotMatching r1 r2 = semiDiff r2 r1

semiDiff r1 r2 = dyaOp Algebra.semiDiff r1 r2
leftSemiDiff r1 r2 = dyaOp Algebra.semiDiff r2 r1

times r1 r2 = dyaOp Algebra.times r1 r2

interJoin r1 r2 = dyaOp Algebra.interJoin r1 r2
iJoin r1 r2 = dyaOp Algebra.iJoin r1 r2

union r1 r2 = dyaOp Algebra.union r1 r2

dUnion r1 r2 = dyaOp Algebra.dUnion r1 r2

intersect r1 r2 = dyaOp Algebra.intersect r1 r2

minus r1 r2 = dyaOp Algebra.minus r1 r2

xUnion r1 r2 = dyaOp Algebra.xUnion r1 r2
xMinus r1 r2 = dyaOp Algebra.xMinus r1 r2

isProperSubsetOf r1 r2 = dyaOp Algebra.isProperSubsetOf r1 r2

isSubsetOf r1 r2 = dyaOp Algebra.isProperSubsetOf r1 r2

