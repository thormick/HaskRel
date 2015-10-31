{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- TODO: Rename and possibly relocate this module.

{-|
Module      : Database.HaskRel.NonRel
Description : Non-relational functions unrelated to the relational model
Copyright   : © Thor Michael Støre, 2015
License     : GPL v2 without "any later version" clause
Maintainer  : thormichael át gmail døt com
Stability   : experimental

Peripheral functions without relational closure, and types related to them.
-}

module Database.HaskRel.NonRel (
  -- * Argument duplication
  cp, cp3, cp4, cp5,
  -- * Non-relational projection.
  project2, aProject2, aProject2uw
  ) where

import Data.Set ( Set )
import qualified Data.Set ( map, foldr )
import Data.HList.CommonMain

import Control.Lens.Wrapped ( Wrapped, Unwrapped, _Wrapped' )
import Control.Lens ( (^.) )


cp :: t -> (t, t)
cp a = (a, a)

cp3 :: t -> (t, t, t)
cp3 a = (a, a, a)

cp4 :: t -> (t, t, t, t)
cp4 a = (a, a, a, a)

cp5 :: t -> (t, t, t, t, t)
cp5 a = (a, a, a, a, a)

{-| Projects a relation into a set of pairs where the first element is the
same as the result of "project" and the second the result of "projectAllBut".
-}
project2 ::
     (Ord (HList l), Ord (HList l1), HAllTaggedEq l, HAllTaggedEq l1,
      HRLabelSet l, HRLabelSet l1,
      H2ProjectByLabels (LabelsOf l2) r l l1) =>
     Set (TIP r) -> hlistOrRecord l2 -> Set (TIP l, TIP l1)
project2 r a = Data.Set.map ( tipyProject2 $ labelsOf a ) r

{-| Projects a relation on a single attribute, into a set of pairs where the first element is the remaining attributes and the second element is the attribute in question.
-}
aProject2 :: forall r l v v' .
     (Ord l, Ord (r v'), HOccurs l (r v), HDeleteAtLabel r l v v') =>
     Set (r v) -> Label l -> Set (r v', l)
aProject2 r a = Data.Set.map (\t -> ( hDeleteAtLabel a t, hOccurs t :: l ) ) r

{-| Projects a relation on a single attribute, into a set of pairs where the first element is the remaining attributes and the second element is the value of the attribute in question. -}
aProject2uw :: forall s r v v' .
     (Ord s, Ord (Unwrapped s), Ord (r v'), HOccurs s (r v),
      HDeleteAtLabel r s v v', Wrapped s) =>
     Set (r v) -> Label s -> Set (r v', Unwrapped s)
aProject2uw r a = Data.Set.map (\t -> ( hDeleteAtLabel a t, ( hOccurs t :: s ) ^. _Wrapped' ) ) r

{-
-- | Maps over a set as a list
listMap f r = Data.Set.foldr f [] r

{-| Takes a set of pairs of an r-tuple and a relation, extends the relation
with the r-tuple. -}
extendTRP p = fromList $ Data.Set.foldr (\(a,t) b -> joinRight a t ++ b ) [] p
-}
