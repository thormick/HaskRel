{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances, FlexibleContexts, ScopedTypeVariables #-}

{-|
Module      : Database.HaskRel.Relational.Variable
Description : Relation variable definition and support functions
Copyright   : © Thor Michael Støre, 2015
License     : GPL v2 without "any later version" clause
Maintainer  : thormichael át gmail døt com
Stability   : experimental

Relation variable definition, along with IO level support functions.
-}
module Database.HaskRel.Relational.Variable (
  -- * Relation variable definitions
  Relvar (Relvar, relvarPath), relvarType,
  -- * Relational input
  readRel, readRelvar, hListsToRel,
  -- * Relational output
  writeRelvarBody, writeRelvarBody', showRelStr
  ) where

import Data.HList.CommonMain

import Data.Set ( Set, fromList )
import Data.Typeable ( Typeable )

import Database.HaskRel.HFWTabulation
  ( HFWPresent, HPresentTypedRecAttr, HPresentRecAttr, showHListSetType,
    hfwPrint, hfwPrintTyped, hfwPrintTypedTS )
import Database.HaskRel.Relational.Definition ( Relation, bodyAsList )


-- == Relation variable reference == --

{-| Relation variable reference. This type has a phantom type variable, which
generally calls for the type to be explicity specified:

@ s = Relvar "SuppliersPartsDB/S.rv" :: Relvar '[SNO, SName, Status, City]@
-}
data Relvar (a::[*]) = Relvar { relvarPath :: FilePath }

-- | Gives the type a relvar results in. Note that the value this results in
-- will always be @undefined@.
relvarType :: Relvar a -> Relation a
relvarType rv = undefined


-- TODO: It should be enforced that a given relvar has a given type
-- TODO: This is also afflicted by the issue that showHListSetType was developed for TIPs

instance Typeable a => Show ( Relvar a ) where
    show a = "Relvar \"" ++ relvarPath a ++ "\" :: Relvar" ++ showHListSetType ( relvarType a )


-- | Converts a list of HLists into a relation value.
hListsToRel
  :: (Ord (HList b), RecordValues b,
      HMapAux HList TaggedFn (RecordValuesR b) b) =>
     [HList (RecordValuesR b)] -> Relation b
hListsToRel = fromList . map hMapTaggedFn


{-| Reads a relation value from a string containing HLists of unpacked attribues.

>>> pt$ ( readRel "H[\"S1\",10],H[\"S2\",50]" :: Relation '[SNO,Status])
┌───────────────┬───────────────────┐
│ sno :: String │ status :: Integer │
╞═══════════════╪═══════════════════╡
│ S1            │ 10                │
│ S2            │ 50                │
└───────────────┴───────────────────┘
-}
readRel :: forall b .
     (Ord (HList b), Read (HList (RecordValuesR b)), RecordValues b,
      HMapAux HList TaggedFn (RecordValuesR b) b) =>
     String -> Relation b
readRel s = hListsToRel $ read $ "[" ++ s ++ "]"

-- | Reads a relation value of a given type from a string.
readRel' :: forall b .
     (Ord (HList b), Read (HList (RecordValuesR b)), RecordValues b,
      HMapAux HList TaggedFn (RecordValuesR b) b) =>
     String -> Relation b -> Relation b
readRel' s rt = hListsToRel $ read $ "[" ++ s ++ "]"

-- | Read a relation variable from the file referenced by the first argument
readRelvar
  :: (Ord (HList b), Read (HList (RecordValuesR b)), RecordValues b,
      HMapAux HList TaggedFn (RecordValuesR b) b) =>
     Relvar b -> IO (Relation b)
readRelvar rv = do
    relStr <- readFile ( relvarPath rv )
    return $ readRel relStr


-- | Prints a relation as a list without the outer brackets.
showRelStr :: (Show (HList (RecordValuesR r)), RecordValues r) =>
     Relation r -> String
showRelStr = init . tail . show . bodyAsList

-- TODO: This uses text-processing on the result of a call to show, fix so that
-- it doesn't break if the show-format is changed.
-- | Writes a body of a relvar to a given file
writeRelvarBody :: Show r => FilePath -> r -> IO ()
writeRelvarBody n hll = writeFile n ( init $ tail $ show hll )

-- | Writes a body of a relvar to a given relvar file reference
writeRelvarBody' :: Show r => Relvar rv -> r -> IO ()
writeRelvarBody' rv hll = writeFile ( relvarPath rv ) ( init $ tail $ show hll )

-- == HFWPresent instance for relvars

instance (Ord (HList b), Read (HList (RecordValuesR b)), Typeable b,
      RecordValues b, HMapAux HList TaggedFn (RecordValuesR b) b,
      HFoldr (Mapcar HPresentTypedRecAttr) [[String]] (RecordValuesR b) [[String]],
      HFoldr (Mapcar HPresentRecAttr) [[String]] (RecordValuesR b) [[String]]) =>
         HFWPresent ( Relvar b )
    where
      hfwPrint r' = do
          r <- readRelvar r'
          hfwPrint r
      hfwPrintTypedTS ts r' = do
          r <- readRelvar r'
          hfwPrintTypedTS ts r
