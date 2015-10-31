{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Database.HaskRelTIP.Relational.Definition
Description : Definition and presentation of relational objects in terms of Haskell and HList TIPs
Copyright   : © Thor Michael Støre, 2015
License     : GPL v2 without "any later version" clause
Maintainer  : thormichael át gmail døt com
Stability   : experimental

Definition and presentation of relational objects in terms of Haskell and HList TIPs.
-}
module Database.HaskRelTIP.Relational.Definition
       ( -- * Presentation functions, of both values and types
         rPrint, rPrintTyped, p, pt, HPresentTIP, HPresentTIPTyped,
         -- * Constructors and constants
         rTuple, rTuple', rTuple'', uniRTuple, uniRTuple', uniRTuple'', 
         relation, relation', relation'', uniRelation, uniRelation', uniRelation'', 
         tableDum, tableDee, empty,
         -- * Aliases and convenience classes
         RHdr, RTuple, Relation, Relation', RDyadic, RHdrClass ( rHdr ),
         HWrap(HWrap), HUnwrap(HUnwrap),
         -- * Convenience functions
         hTIPRearrange, renameTIPLabel, unordTIPEq, unwrapField,
         unConsTagWrapU, rTupleToHList, bodyAsList ) where

import Database.HaskRelTIP.TIPFWTabulation

import Database.HaskRel.Relational.Definition ( empty, p, pt, rPrint, rPrintTyped )

import Control.Lens ( (^.), from )
import Control.Lens.Wrapped ( Wrapped, Unwrapped, _Wrapped', _Unwrapped' )

import Data.Set ( Set, fromList, foldr )
import qualified Data.Set ( empty )

import Data.HList.CommonMain
import Data.HList.TIP (unTIP)

import Data.Coerce


type RHdr a = TIP ( TagR a )
type RTuple a = RHdr a

type Relation a = Set ( RTuple a )

type Relation' a = Set ( TIP a )

-- (x1 -> a1) ( TIP ( '[Tagged a1 a1] ) )
{-
instance HTuple '[Tagged a (a -> a')] (Tagged a (a -> a')) where
    hToTuple (a `HCons` HNil) = (a)
    hFromTuple (a) = (a `HCons` HNil)
instance HTuple '[Tagged a a'] (Tagged a a') where
    hToTuple (a `HCons` HNil) = (a)
    hFromTuple (a) = (a `HCons` HNil)
-}

-- Enables expressing rHdr (Foo, Bar) instead of (undefined :: RHdr '[Foo, Bar])
class RHdrClass a b | a -> b where
   rHdr :: a -> b

instance RHdrClass () ( TIP ( '[] ) )
  where
    rHdr _ = undefined :: RHdr '[]

instance RHdrClass (x1 -> a1) ( TIP ( '[Tagged a1 a1] ) )
  where
    rHdr _ = undefined :: RHdr '[a1]

instance RHdrClass (x1 -> a1, x2 -> a2) ( TIP ( '[Tagged a1 a1, Tagged a2 a2] ) )
  where
    rHdr _ = undefined :: RHdr '[a1, a2]

instance RHdrClass (x1 -> a1, x2 -> a2, x3 -> a3)
                   ( TIP ( '[Tagged a1 a1, Tagged a2 a2, Tagged a3 a3] ) )
  where
    rHdr _ = undefined :: RHdr '[a1, a2, a3]

instance RHdrClass (x1 -> a1, x2 -> a2, x3 -> a3, x4 -> a4)
                   ( TIP ( '[Tagged a1 a1, Tagged a2 a2, Tagged a3 a3, Tagged a4 a4] ) )
  where
    rHdr _ = undefined :: RHdr '[a1, a2, a3, a4]

instance RHdrClass (x1 -> a1, x2 -> a2, x3 -> a3, x4 -> a4, x5 -> a5)
                   ( TIP ( '[Tagged a1 a1, Tagged a2 a2, Tagged a3 a3, Tagged a4 a4, Tagged a5 a5] ) )
  where
    rHdr _ = undefined :: RHdr '[a1, a2, a3, a4, a5]

-- TODO: Etc. ad nauseam. Would very much like to see a general solution to this.

-- Element level functions

data HWrap = HWrap
instance ( a ~ Unwrapped b, Wrapped b ) => ApplyAB HWrap a b where
    applyAB _ a = a ^. _Unwrapped'

data HUnwrap = HUnwrap
instance ( b ~ Unwrapped a, Wrapped a ) => ApplyAB HUnwrap a b where
    applyAB _ a = a ^. _Wrapped'

-- TIP level functions

-- Having to convert this back and forth between an HList to rearrange the
-- elements feels awkward...
hTIPRearrange :: (TagUntagFD a ta, TagUntagFD a1 l, HProject (HList a) (HList a1)) =>
     TIP ta -> TIP l
hTIPRearrange t = ( hProject $ unTagTIP t ) ^. from tipHList

renameTIPLabel :: forall a b l r v' v.
    ( HOccurs a l, HExtend b l, HDeleteAtLabel r a v v', HExtendR b l ~ r v,
      Wrapped a, Wrapped b, Unwrapped a ~ Unwrapped b ) =>
        a -> b -> l -> r v'
renameTIPLabel _ _ t =
    hDeleteAtLabel
        ( undefined :: Label a ) $
        ( ( hOccurs t :: a ) ^. _Wrapped' ^. _Unwrapped' :: b ) .*. t

{-
-- TODO: Use .!. and HasField instead?
renameTIPLabel' :: forall a b l r v' v x.
    ( HasField a l x, HExtend b l, HDeleteAtLabel r a v v', HExtendR b l ~ r v,
      Wrapped b, Wrapped x, Unwrapped x ~ Unwrapped b ) =>
        Label a -> Label b -> l -> r v'
renameTIPLabel' a _ t = hDeleteAtLabel a ( ( ( ( t .!. a ) ^. _Wrapped' ) ^. _Unwrapped' :: b ) .*. t )
-}

{-| Order-agnostic equality operation, which is neccessary for comparison correct for r-tuples

TODO: Having a separate notion of equality gives rise to complications. Is it possible to have this an instance of Eq?

>>> fbbRel == fbbRel2
False
>>> fbbRel == fbbRelX
... Couldn't match type ‘Bar’ with ‘Foo’ ...
>>> fbbRel == ( relRearrange fbbRelX )
True
-}
unordTIPEq ::
     (Eq (HList a), TagUntagFD a ta, TagUntagFD a1 ta1,
      HProject (HList a1) (HList a)) =>
     TIP ta -> TIP ta1 -> Bool
unordTIPEq l r = unTagTIP l == ( hProject $ unTagTIP r )
-- tipyProject projects without rearranging, so hProject is employed instead.

unwrapField :: (HasField l r s, Wrapped s) => Label l -> r -> Unwrapped s
unwrapField a l = l .!. a ^. _Wrapped'

{-| Untag, untip, extract the single element from the TIP, and finally unwrap the resulting value.
-}
unConsTagWrapU :: Wrapped y => TIP '[Tagged y y] -> Unwrapped y
unConsTagWrapU a = unConsTagWrapU' $ unTagTIP a

unConsTagWrapU' :: Wrapped s => HList '[s] -> Unwrapped s
unConsTagWrapU' (a `HCons` HNil) = a ^. _Wrapped'

-- | Unwrap an r-tuple into a hlist
rTupleToHList :: (TagUntagFD a ta, HMapCxt HList HUnwrap a b) =>
      TIP ta -> HList b
rTupleToHList = hMapL HUnwrap . hUntagSelf . unTIP

{-| Convenience class for more concise @Relation r1 -> Relation r2 -> Relation r1@ functions, where relRearrange may be applied to either r1 or r2 to make them the same type. The basic class constraints those functions have in common is:

@ TagUntagFD r1' r1, TagUntagFD r2' r2, HProject (HList r1') (HList r2') @

Additionally, all tuples of all relations must be comparable, and the attributes of all relations must always form a set. Therefore, even for cases where it is not a requirement, it is acceptable or beneficial to add:

@ Eq (HList r2'), HAllTaggedEq r1, HAllTaggedEq r2, HRLabelSet r1, HRLabelSet r2 @

Adding @Ord (HList r2)@ would also be an option, but that is beyond relational theory and as such something I'd prefer to avoid.
-}
class ( TagUntagFD r1' r1, TagUntagFD r2' r2, HProject (HList r1') (HList r2'),
        Eq (HList r2'), HAllTaggedEq r1, HAllTaggedEq r2,
        HRLabelSet r1, HRLabelSet r2
      ) => RDyadic r1 r2 r1' r2'

instance ( TagUntagFD r1' r1, TagUntagFD r2' r2, HProject (HList r1') (HList r2'),
           Eq (HList r2'), HAllTaggedEq r1, HAllTaggedEq r2,
           HRLabelSet r1, HRLabelSet r2
         ) => RDyadic r1 r2 r1' r2'

-- Constructor-type functions

{- NOTE: hFromTuple only supports 0 and 2-6 tuples, which is quite unfortunate.
Because of limits in Haskell the following won't work:

instance HTuple '[a] (a) where
    hToTuple (a `HCons` HNil) = (a)
    hToTuple _ = error "HTuple impossible"
    hFromTuple (a) = (a `HCons` HNil)

This is quite problematic for the implementation of rTuple and rTuple', and is the
unfortunate reason for the existence of uniRTuple and uniRTuple'.
-}

rTuple :: (TagUntagFD v ta, HTuple v t) => t -> TIP ta
rTuple t = ( hFromTuple t ) ^. from tipHList'

rTuple' :: (TagUntagFD b ta, HTuple a t, HMapCxt HList HWrap a b) =>
     t -> TIP ta
rTuple' t = ( hMapL HWrap ( hFromTuple t ) ) ^. from tipHList'

rTuple'' :: (TagUntagFD v ta, HTuple v t, Coercible a t) => a -> TIP ta
rTuple'' t = ( hFromTuple $ coerce t ) ^. from tipHList'


uniRTuple :: t -> TIP '[Tagged t t]
uniRTuple a = a .*. emptyTIP

uniRTuple' :: Wrapped y => Unwrapped y -> TIP '[Tagged y y]
uniRTuple' a = ( hMapL HWrap ( a .*. HNil ) ) ^. from tipHList'
-- Instead of having to lens it up it would be nice to do something like:
-- hMap HWrap ( a .*. emptyTIP ) :: TIP ( TagR '[a] )
-- But that requires a hMap for TIPs, of course.

uniRTuple'' :: Coercible a y => a -> TIP '[Tagged y y]
uniRTuple'' a = ( ( coerce a ) .*. HNil ) ^. from tipHList'

-- TODO: Verify: "relation" supports both a list of HTuples (Haskell tuples as HLists) and of
-- RTuples (HList TIP types).
relation ::
     (Ord (HList ta), TagUntagFD v ta, HTuple v t, HRLabelSet ta) =>
     [t] -> Relation' ta
relation hTIPList = fromList $ map rTuple hTIPList

relation' ::
     (Ord (HList ta), TagUntagFD b ta, HTuple a t, HMapCxt HList HWrap a b,
      HRLabelSet ta) =>
     [t] -> Relation' ta
relation' tupList = fromList $ map rTuple' tupList

-- TODO: Instances for printing coercible relation values
relation'' ::
     (Ord (HList ta), TagUntagFD v ta, HTuple v t, Coercible a t,
      HRLabelSet ta) =>
     [a] -> Relation' ta
relation'' tupList = fromList $ map rTuple'' tupList

{- With OverloadedLists this would work the same as "relation":
relation''' ::
     (Ord (HList ta), TagUntagFD v ta, HTuple v t,
      HRLabelSet ta) =>
     Set t -> Relation' ta
relation''' hTIPList = Data.Set.map rTuple hTIPList
-}

uniRelation :: Ord e => [e] -> Relation '[e]
uniRelation uniTupList = fromList $ map uniRTuple uniTupList

uniRelation' :: (Ord y, Wrapped y) => [Unwrapped y] -> Relation '[y]
uniRelation' uniTupList = fromList $ map uniRTuple' uniTupList

uniRelation'' :: (Ord y, Coercible a y) => [a] -> Relation '[y]
uniRelation'' uniTupList = fromList $ map uniRTuple'' uniTupList


-- | Unwraps a relation body into a list of HLists
bodyAsList ::
     (Ord (HList b), TagUntagFD a ta, HMapCxt HList HUnwrap a b) =>
      Relation' ta -> [HList b]
bodyAsList r = Data.Set.foldr (\t b -> ( rTupleToHList t ) : b ) [] r


-- Constants

{- | The nullary relation with an empty body

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

{- | The nullary relation of cardinality one, HList TIP based.

>>> pt$ tableDee
┌──┐
│  │
╞══╡
│  │
└──┘
>>> relation [()] == tableDee
True
-}
tableDee :: Relation '[]
tableDee = fromList[emptyTIP]

{- Only when something akin to
      >>> relation [(Foo 5, Bar "asdf")] `union` empty
works is this satisfactory.
Annoyingly enough:
rPrint$ ( relation [(Foo 5, Bar "asdf")] ) `Data.Set.union` empty
┌─────┬──────┐
│ Foo │ Bar  │
╞═════╪══════╡
│ 5   │ asdf │
└─────┴──────┘
-}
