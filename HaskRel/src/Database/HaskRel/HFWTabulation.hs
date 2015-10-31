{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

-- TODO: Can this be rewritten to be less reliant on Data.Typeable? Should be possible to just use it for getting the field type names, and not for parsing the entire type.
-- TODO: Check that nothing is missing from the export list; only HaskRel's dependencies are known to be covered.
-- TODO: New functions that did a tiny bit more than the last ones have been added haphazardly, clean up this and the users of this.

{-|
Module      : HFWTabulation
Description : Presentation of HList values in a two-dimensional fixed-width font form.
Copyright   : © Thor Michael Støre, 2015
License     : GPL v2 without "any later version" clause
Maintainer  : thormichael át gmail døt com
Stability   : experimental

HList fixed-width tabular presentation. Presentation of HList values in a two-dimensional, tabular fixed-width font form with a header consisting of labels and optionally types. Only records are supported by this module, see `TIPFWTabulation` for support for TIPs.
-}
module Database.HaskRel.HFWTabulation (
  HFWPresent ( hfwPrint, hfwPrintTyped, hfwPrintTypedTS ),
  FWPresent ( fwPresent, fwPresentTyped ),
  FWPresent' ( fwPresent', fwPresentTyped' ),
  printHRecSetTab, printHRecSetTabTyped, printHRecSetTabTypedTS,
  showHRecSetTab,
  showTR, showTRTS, showHTypeTS, showHListSetType,
  HFWTIPSet, HFWTIP, HFWRec, HFWString, HFWOther,
  HListTypeSynonym ( hRecTS, hRecSetTS, hTIPTS, hTIPSetTS ), FWPPred,
  HPresentRecAttr(HPresentRecAttr), HPresentTypedRecAttr(HPresentTypedRecAttr)
  ) where

import Data.HList.CommonMain

import Data.Typeable

import Data.Set ( Set, toList )
import Data.List ( intercalate )

import Database.HaskRel.FWTabulation


-- Presentation of table heading

-- data HFWTIPList -- <- TODO. Use ↑ to distinguish them from sets? Not really appropriate, it's not known to be ordered. [] in some fashion?
data HFWTIPSet
data HFWTIP
-- data HFWRecList -- <- TODO
data HFWRecSet
data HFWRec
data HFWString
data HFWOther

type family FWPPred a where
--    FWPPred [ Record a ] = HFWRecList
    FWPPred ( Set ( Record a ) ) = HFWRecSet
    FWPPred ( Record a )         = HFWRec
--    FWPPred [ ( TIP a ) ]    = HFWTIPList
    FWPPred ( Set ( TIP a ) )    = HFWTIPSet
    FWPPred ( TIP a )            = HFWTIP
    FWPPred String               = HFWString
    FWPPred a                    = HFWOther


-- As having no type synonyms in effect
data EmptyTS = EmptyTS

-- | Type synoyms used when building the table header with type names
class HListTypeSynonym s where
  hRecTS :: s -> String
  hRecSetTS :: s -> String
  hTIPTS :: s -> String
  hTIPSetTS :: s -> String

-- TODO: This only supports showing a single type synonym, it doesn't support showing "Set ( TIP '[Foo] )", for instance. This is sufficient for HaskRel, which only uses full type synonyms, for instance "Relation '[Foo]".
instance HListTypeSynonym EmptyTS where
  hRecTS _ = "Record"
  hRecSetTS _ = "Set-Record"
  hTIPTS _ = "TIP"
  hTIPSetTS _ = "Set-TIP"


-- | Show a TypeRep
showTR :: TypeRep -> String
showTR = showTRTS EmptyTS

-- TODO: Record presentation is copy'n'paste of TIP presentation, which gives the unfortunate 'Record '["foo"]'
-- | Show a TypeRep, using the given type synonyms
showTRTS :: HListTypeSynonym ts => ts -> TypeRep -> String
showTRTS ts t
  | t == stringType   = "String"
  | tyCon == recTyCon = hRecTS ts ++ showHListType app
  | tyCon == tipTyCon = hTIPTS ts ++ showHListType app
  | tyCon == setTyCon && typeRepTyCon ( head app ) == recTyCon =
      hRecSetTS ts ++ showHListType ( typeRepArgs $ head app )
  | tyCon == setTyCon && typeRepTyCon ( head app ) == tipTyCon =
      hTIPSetTS ts ++ showHListType ( typeRepArgs $ head app )
  | otherwise         = show t
  where
    ( tyCon, app ) = splitTyConApp t
    stringType = typeRep ( Proxy :: Proxy String )
    -- The argument to Set could be anything that is an instance of Typeable
    setTyCon = typeRepTyCon $ typeRep ( Proxy :: Proxy ( Set Int ) )
    -- listTyCon = ...
    recTyCon = typeRepTyCon $ typeRep ( Proxy :: Proxy ( Record '[] ) )
    tipTyCon = typeRepTyCon $ typeRep ( Proxy :: Proxy ( TIP '[] ) )

showHListSetType :: forall a (r :: [*] -> *) .
    ( Typeable r, Typeable a ) =>
    Set (r a) -> String
showHListSetType = showHListType . typeRepArgs . head . typeRepArgs . typeOf

showHListType :: [TypeRep] -> String
showHListType = (++) " '" . show . hListTypeToTRList

hListTypeToTRList :: [TypeRep] -> [TypeRep]
hListTypeToTRList = parseHListType . typeRepArgs . head

parseHListType :: [TypeRep] -> [TypeRep]
parseHListType [] = []
parseHListType [_] = error "Not a valid TIP/Record type" -- Always come in pairs. Will also fail here if it's not tagged
parseHListType (tr:trx) = head ( typeRepArgs tr ) : parseHListType ( typeRepArgs $ head trx )


-- Meta information

tagFName :: TypeRep -> String
tagFName = tail . init . show . head . typeRepArgs

{-|
>>> hRecFNames $ sno .=. "S1" .*. status .=. 10 .*. emptyRecord
["sno","status"]
-}
hRecFNames :: Typeable a => a -> [String]
hRecFNames = map tagFName . flatHRec

tagQFName :: TypeRep -> String
tagQFName = tagQFNameTS EmptyTS

tagQFNameTS :: HListTypeSynonym ts => ts -> TypeRep -> String
tagQFNameTS ts a = tagFName a ++ " :: " ++ showHTypeTS ts a

{-|
>>> hRecQFNames $ sno .=. "S1" .*. status .=. 10 .*. emptyRecord
["sno :: String","status :: Integer"]
-}
hRecQFNames :: Typeable a => a -> [String]
hRecQFNames = map tagQFName . flatHRec

hRecQFNamesTS :: ( HListTypeSynonym ts, Typeable a ) => ts -> a -> [String]
hRecQFNamesTS ts = map ( tagQFNameTS ts ) . flatHRec

showHTypeTS :: HListTypeSynonym ts => ts -> TypeRep -> String
showHTypeTS ts = showTRTS ts . head . tail . typeRepArgs


{-
>>> typeOf $ sno .=. "S1" .*. status .=. 10 .*. emptyRecord
Record (: * (Tagged Symbol "sno" [Char]) (: * (Tagged Symbol "status" Integer) []))
>>> flatHRec $ sno .=. "S1" .*. status .=. 10 .*. emptyRecord
[Tagged Symbol "sno" [Char],Tagged Symbol "status" Integer]
-}
-- I did this before noticing hEnd, would that in the right location work just as well?
-- TODO: Rewrite to use typeRep and Proxy. (This probably goes for other places too.)
flatHRec :: Typeable a => a -> [TypeRep]
flatHRec = flatHRec' . typeRepArgs . head . typeRepArgs . typeOf

flatHRec' :: [TypeRep] -> [TypeRep]
flatHRec' [] = []
flatHRec' [m] = [m]
flatHRec' (m:mx) = m : flatHRec' ( typeRepArgs $ head mx )


-- Presentation

class HFWPresent r where
    hfwPrint :: r -> IO ()
    hfwPrintTyped :: r -> IO ()
    hfwPrintTyped = hfwPrintTypedTS EmptyTS
    hfwPrintTypedTS :: HListTypeSynonym ts => ts -> r -> IO ()

class Show a => FWPresent a where
    fwPresent      :: a -> [String]
    fwPresentTyped :: a -> [String]

class Show a => FWPresent' flag a where
    fwPresent'      :: flag -> a -> [String]
    fwPresentTyped' :: flag -> a -> [String]


-- Presentation of a non-HList value

instance (FWPPred a ~ flag, FWPresent' flag a) => FWPresent a where
    fwPresent      = fwPresent' ( undefined :: flag )
    fwPresentTyped = fwPresentTyped' ( undefined :: flag )

instance Show a => FWPresent' HFWOther a where
    fwPresent' _ x      = [show x]
    fwPresentTyped' _ x = [show x]

instance FWPresent' HFWString String where
    fwPresent' _ x      = [x]
    fwPresentTyped' _ x = [x]


-- Presentation of a single record

buildHRec
  :: (Typeable r, RecordValues r,
      HFoldr (Mapcar HPresentRecAttr) [[String]] (RecordValuesR r) [[String]]) =>
     Record r -> [String]
buildHRec rrTup = present1LineValue ( listPresentRec rrTup ) ( hRecFNames rrTup )

buildHRecTyped
  :: (Typeable r, RecordValues r,
      HFoldr (Mapcar HPresentTypedRecAttr) [[String]] (RecordValuesR r) [[String]]) =>
     Record r -> [String]
buildHRecTyped rrTup = present1LineValue ( listPresentTypedRec rrTup ) ( hRecQFNames rrTup )

buildHRecTypedTS
  :: (HListTypeSynonym ts, Typeable r, RecordValues r,
      HFoldr (Mapcar HPresentTypedRecAttr) [[String]] (RecordValuesR r) [[String]]) =>
     ts -> Record r -> [String]
buildHRecTypedTS ts rrTup = present1LineValue ( listPresentTypedRec rrTup ) ( hRecQFNamesTS ts rrTup )

listPresentRec :: (RecordValues r,
      HFoldr (Mapcar HPresentRecAttr) [e] (RecordValuesR r) [e]) =>
     Record r -> [e]
listPresentRec = hMapOut HPresentRecAttr . recordValues

listPresentTypedRec :: (RecordValues r,
      HFoldr (Mapcar HPresentTypedRecAttr) [e] (RecordValuesR r) [e]) =>
     Record r -> [e]
listPresentTypedRec = hMapOut HPresentTypedRecAttr . recordValues


data HPresentRecAttr = HPresentRecAttr
instance ([String] ~ stringL, FWPresent' (FWPPred a) a) => 
        ApplyAB HPresentRecAttr a stringL
    where
        applyAB _ = fwPresent

data HPresentTypedRecAttr = HPresentTypedRecAttr
instance ([String] ~ stringL, FWPresent' (FWPPred a) a) => 
        ApplyAB HPresentTypedRecAttr a stringL
    where
        applyAB _ = fwPresentTyped


instance (HFoldr (Mapcar HPresentRecAttr) [[String]] (RecordValuesR r) [[String]],
          HFoldr (Mapcar HPresentTypedRecAttr) [[String]] (RecordValuesR r) [[String]],
          Typeable r, RecordValues r
          ) =>
         HFWPresent ( Record r )
    where
        hfwPrint = putStrLn . intercalate "\n" . buildHRec
        hfwPrintTyped = putStrLn . intercalate "\n" . buildHRecTyped
        hfwPrintTypedTS ts = putStrLn . intercalate "\n" . buildHRecTypedTS ts


instance ( HFoldr (Mapcar HPresentRecAttr) [[String]] (RecordValuesR r) [[String]],
           HFoldr (Mapcar HPresentTypedRecAttr) [[String]] (RecordValuesR r) [[String]],
           Typeable r, RecordValues r, ShowComponents r
         ) =>
        FWPresent' HFWRec ( Record r )
  where
    fwPresent' _      = buildHRec
    fwPresentTyped' _ = buildHRecTyped

-- Presentation of a set of records

showHRecSetTab ::
     (Typeable a, RecordValues a,
      HFoldr (Mapcar HPresentRecAttr) [[String]] (RecordValuesR a) [[String]]) =>
     Set (Record a) -> String
showHRecSetTab = intercalate "\n" . buildHRecSet

{-
showHRecSetTabTyped = intercalate "\n" . buildHRecSetTyped
showHRecSetTabTypedTS = intercalate "\n" . buildHRecSetTypedTS
-}

-- | Prints a set of HList records in a table format
printHRecSetTab a = putStrLn $ intercalate "\n" $ buildHRecSet a
-- | Prints a set of HList records in a table format, with types in the header
printHRecSetTabTyped a = putStrLn $ intercalate "\n" $ buildHRecSetTyped a
-- | Prints a set of HList records in a table format, with types that use the given type synonyms in the header
printHRecSetTabTypedTS ts a = putStrLn $ intercalate "\n" $ buildHRecSetTypedTS ts a

instance (Typeable a, RecordValues a,
          HFoldr (Mapcar HPresentRecAttr) [[String]] (RecordValuesR a) [[String]],
          HFoldr (Mapcar HPresentTypedRecAttr) [[String]] (RecordValuesR a) [[String]]) =>
      HFWPresent ( Set ( Record a ) ) where
      hfwPrint = printHRecSetTab
      hfwPrintTyped = printHRecSetTabTyped
      hfwPrintTypedTS = printHRecSetTabTypedTS

instance (Typeable a, RecordValues a, ShowComponents a,
          HFoldr (Mapcar HPresentRecAttr) [[String]] (RecordValuesR a) [[String]],
          HFoldr (Mapcar HPresentTypedRecAttr) [[String]] (RecordValuesR a) [[String]]) =>
        FWPresent' HFWRecSet ( Set ( Record a ) )
  where
    fwPresent' _      = buildHRecSet
    fwPresentTyped' _ = buildHRecSetTyped

unwrap :: x (Record a) -> Record a
unwrap = undefined

buildHRecSet ::
     (Typeable a, RecordValues a,
      HFoldr (Mapcar HPresentRecAttr) [[String]] (RecordValuesR a) [[String]]) =>
     Set (Record a) -> [String]
buildHRecSet rs =
  presentNLineValue ( map listPresentRec $ toList rs ) ( hRecFNames $ unwrap rs )

buildHRecSetTyped ::
     (Typeable a, RecordValues a,
      HFoldr (Mapcar HPresentTypedRecAttr) [[String]] (RecordValuesR a) [[String]]) =>
     Set (Record a) -> [String]
buildHRecSetTyped rs =
  presentNLineValue ( map listPresentTypedRec $ toList rs ) ( hRecQFNames $ unwrap rs )

buildHRecSetTypedTS
  :: (Typeable a, RecordValues a,
      HFoldr
        (Mapcar HPresentTypedRecAttr)
        [[String]]
        (RecordValuesR a)
        [[String]],
      HListTypeSynonym ts) =>
     ts -> Set (Record a) -> [String]
buildHRecSetTypedTS ts rs =
  presentNLineValue ( map listPresentTypedRec $ toList rs ) ( hRecQFNamesTS ts $ unwrap rs )
