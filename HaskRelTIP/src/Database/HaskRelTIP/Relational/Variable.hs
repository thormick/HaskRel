{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{-|
Module      : Database.HaskRelTIP.Relational.Variable
Description : Relation variable definition and support functions
Copyright   : © Thor Michael Støre, 2015
License     : GPL v2 without "any later version" clause
Maintainer  : thormichael át gmail døt com
Stability   : experimental

Relation variable definition, along with IO level support functions
-}
module Database.HaskRelTIP.Relational.Variable (
  -- * Relation variable definitions
  Relvar (Relvar, relvarPath), relvarType,
  -- * Relational input
  readRel, readRelvar, readRelvar', readHListsToRel,
  -- * Relational output
  writeRelvarBody, writeRelvarBody', showRelStr
  ) where

import Control.Lens ( (^.), from )
import Control.Lens.Wrapped ( Unwrapped, _Wrapped', _Unwrapped' )

import Data.HList.CommonMain

import Data.Set ( fromList )
import Data.Typeable ( Typeable )

import Database.HaskRel.HFWTabulation ( showHListSetType )
import Database.HaskRelTIP.Relational.Definition
  ( HWrap ( HWrap ), HUnwrap, Relation', bodyAsList )


-- == Relation variable reference == --
{-| Relation variable reference. Should be declared without a value for the second argument, only the type is used:

@ s = Relvar "SuppliersPartsDB/S.rv" ( undefined :: Relation '[SNO, SName, Status, City] )@
-}
data Relvar (a :: [*]) = Relvar { relvarPath :: FilePath }

-- | Gives the type a relvar results in. Note that the value this results in will always be @undefined@.
relvarType :: Relvar a -> Relation' a
relvarType rv = undefined


-- TODO: It should be enforced that a given relvar has a given type

instance Typeable a => Show ( Relvar a ) where
    show a = "Relvar \"" ++ relvarPath a ++ "\" (undefined::" ++ ( showHListSetType $ relvarType a ) ++ ")"


-- | Read a relation value
readRel :: forall a b ta . (Read (HList a), Ord (HList ta),
         TagUntagFD b ta, HMapCxt HList HWrap a b) =>
     String -> Relation' ta
readRel s = readHListsToRel ( read ("[" ++ s ++ "]") :: [HList a] )

readRel' :: forall a b ta . (Read (HList a), Ord (HList ta),
         TagUntagFD b ta, HMapCxt HList HWrap a b) =>
     String -> HList a -> Relation' ta
readRel' s _ = readHListsToRel ( read ("[" ++ s ++ "]") :: [HList a] )

{-
*RelExample> pt ( readRel "H[5,\"asdf\"],H[6,\"qwerty\"]" :: Relation '[Foo, Bar] )
┌───────────┬──────────────┐
│ Foo : Int │ Bar : String │
╞═══════════╪══════════════╡
│ 5         │ "asdf"       │
│ 6         │ "qwerty"     │
└───────────┴──────────────┘
-}

tipWrap :: (TagUntagFD b ta, HMapCxt HList HWrap a b) =>
     HList a -> TIP ta
tipWrap h = ( hMapL HWrap h ) ^. from tipHList'

readHListsToRel ::
     (Ord (HList ta), TagUntagFD b ta, HMapCxt HList HWrap a b) =>
     [HList a] -> Relation' ta
readHListsToRel hlistArr = fromList $ map tipWrap hlistArr

-- | Read a relation variable from the file referenced by 'Relvar ta'
readRelvar :: forall a b ta .
    (Read (HList a), Ord (HList ta), TagUntagFD b ta,
     HMapCxt HList HWrap a b) =>
    Relvar ta -> IO (Relation' ta)
readRelvar rv = do
    relStr <- readFile ( relvarPath rv )
    return $ readRel' relStr ( undefined :: HList a )

{-|
Variant of readRelvar that takes an explicit argument for the relvar file data
format.
TODO: Is this required, or can HList a and Relvar ta be ~?
-}
readRelvar' :: -- forall a b ta .
     (Read (HList a), Ord (HList ta), TagUntagFD b ta,
      HMapCxt HList HWrap a b) =>
     Relvar ta -> HList a -> IO (Relation' ta)
readRelvar' rv hl = do
    relStr <- readFile ( relvarPath rv )
    return $ readRel' relStr hl -- or just ( undefined :: HList a )

{-
With "FooBarBaz.rv" being a file containing
H[5,"asdf","qwer"],H[5,"sdfg","wert"],H[7,"aeruwfht","zcxbv"],H[7,"oxzcij","iofjwa"],H[7,"werg","hotf"]

*RelExample> :{
do
    putStrLn ""
    fbbRel' <- readRelvar ( Relvar "FooBarBaz.rv" ( undefined :: Relation '[Foo, Bar, Baz] ) )
    pt$ fbbRel'
:}
*RelExample| *RelExample| *RelExample| *RelExample| 
┌───────────┬──────────────┬──────────────┐
│ Foo : Int │ Bar : String │ Baz : String │
╞═══════════╪══════════════╪══════════════╡
│ 5         │ "asdf"       │ "qwer"       │
│ 5         │ "sdfg"       │ "wert"       │
│ 7         │ "aeruwfht"   │ "zcxbv"      │
│ 7         │ "oxzcij"     │ "iofjwa"     │
│ 7         │ "werg"       │ "hotf"       │
└───────────┴──────────────┴──────────────┘

Putting the type on a use of the relation variable also works:
  do
    fbbRel' <- readRelvar fooBarBaz
    pt$ ( fbbRel' :: Relation '[Foo, Bar, Baz] )
Or with ScopedTypeVariables:
    ( fbbRel' :: Relation '[Foo, Bar, Baz] ) <- readRelvar "FooBarBaz.rv"
-}

-- | Prints a relation as a list without the outer brackets.
showRelStr :: forall a b ta .
     (Show (HList b), Ord (HList b), TagUntagFD a ta,
      HMapCxt HList HUnwrap a b) =>
     Relation' ta -> String
showRelStr r = init $ tail $ show $ ( bodyAsList r :: [HList b] )
-- TODO: This uses text-processing on the result of a call to show, fix so that
-- it doesn't break if the show-format is changed.

writeRelvarBody :: Show r => FilePath -> r -> IO ()
writeRelvarBody n hll = writeFile n ( init $ tail $ show $ hll )

writeRelvarBody' :: Show r => Relvar rv -> r -> IO ()
writeRelvarBody' rv hll = writeFile ( relvarPath rv ) ( init $ tail $ show $ hll )
