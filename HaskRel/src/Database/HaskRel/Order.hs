{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : Database.HaskRel.Order
Description : Ordering functions
Copyright   : © Thor Michael Støre, 2015
License     : GPL v2 without "any later version" clause
Maintainer  : thormichael át gmail døt com
Stability   : experimental

Ordering functions. This is not of the relational model.
-}

module Database.HaskRel.Order (
  -- * Ordering functions.
  orderBy, orderOn,
  Asc(Asc), Desc(Desc)
  ) where

import Data.Set ( Set, toList )
import qualified Data.Set
import Data.List ( sortOn, sortBy )
import Data.HList.CommonMain

-- import Database.HaskRel.Relational.TIP.Definition
-- import Data.Ord ( Down(Down) )

{- Converts a relation to an ordered set of r-tuples, ordered ascendingly. Because
Data.Set orders its elements (which is a no-no in relational theory and a
quality we must disregard in the context of it), and as such functions as an
ordered set, this will just rearrange the attributes.

ordRTupSet ::
     (Ord (HList l), TagUntagFD a ta, TagUntagFD a1 l,
      HProject (HList a) (HList a1)) =>
     Relation' ta -> Relation' l -> Relation' l
ordRTupSet r _ = Data.Set.map hTIPRearrange r
TODO: This is TIP specific, figure out if this is neccessary or beneficial at all.
-}

{-| Takes any set (including that of a relation) and results in a list of the
elements it consists of ordered by the given predicate
-}
orderBy :: Set a -> (a -> a -> Ordering) -> [a]
orderBy r p = sortBy p $ toList r

{-| Takes any set (including that of a relation) and results in a list of the
elements it consists of ordered on the given key function
-}
orderOn :: Ord b => Set a -> (a -> b) -> [a]
orderOn r k = sortOn k $ toList r

newtype Asc a = Asc a deriving (Show, Read, Eq, Ord)
-- As with Data.Ord.Down
newtype Desc a = Desc a deriving (Show, Read, Eq)

instance Ord a => Ord (Desc a) where
    compare (Desc x) (Desc y) = y `compare` x
