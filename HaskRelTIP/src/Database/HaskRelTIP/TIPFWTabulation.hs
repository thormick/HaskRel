{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Database.HaskRelTIP.TIPFWTabulation
Description : Presentation of HList TIPs in a two-dimensional fixed-width font form.
Copyright   : © Thor Michael Støre, 2015
License     : GPL v2 without "any later version" clause
Maintainer  : thormichael át gmail døt com
Stability   : experimental

HList TIP fixed-width tabular presentation. Presentation of HList TIP values in a two-dimensional, tabular fixed-width font form. This is separated out since TIPs require a bit more plumbing, and particularly lens, which records don't need for simple presentation.
-}
module Database.HaskRelTIP.TIPFWTabulation (
  HPresentTIP, HPresentTIPTyped,
  showTIPSetTab, printTIPSet, printTIPSetTyped, printTIPSetTypedTS,
  -- * Utility functions
  unTagTIP ) where

import Data.HList.CommonMain

import Data.Typeable
import Data.Data

import Data.Set ( Set, toList )
import Data.List ( intercalate )

import Control.Lens.Wrapped ( Wrapped, Unwrapped, _Wrapped' )
import Control.Lens ( (^.) )

import Database.HaskRel.FWTabulation
import Database.HaskRel.HFWTabulation

import Data.HList.TIP ( unTIP )


-- == Processing individual fields == --

getAlgCtor :: Data a => a -> Constr
getAlgCtor x = case dataTypeRep $ dataTypeOf x
               of ( AlgRep cs ) -> getSingle cs
                  _ -> error "Not an algebraic type"

-- Elements of TIPs must have a single data constructor.
getSingle :: [a] -> a
getSingle [] = error "No data constructor found"
getSingle [x] = x
getSingle (_:_) = error "Multiple data constructors found where one expected"

newtype DTInfo a = DTInfo [TypeRep]

attrTyInfo :: forall a. Data a => a -> [TypeRep]
attrTyInfo t = attrTyInfo' t ( getAlgCtor t )

attrTyInfo' :: forall a. Data a => a -> Constr -> [TypeRep]
attrTyInfo' _ ctor = case go ctor of DTInfo reps -> reverse reps
  where
  go :: Constr -> DTInfo a
  go = gunfold (\ (DTInfo infos :: DTInfo (b->r)) ->
                  DTInfo (typeRep (Proxy :: Proxy b) : infos))
               (\_ -> DTInfo [])


-- == Fundamentals and presentation of TIP values == --

-- Untag, untip TIP
unTagTIP :: TagUntagFD a ta => TIP ta -> HList a
unTagTIP = hUntagSelf . unTIP

-- TODO: Fix orphan instances

instance (HFoldr (Mapcar HPresentTIP) [[String]] a [[String]],
          HFoldr (Mapcar HPresentTIPTyped) [[String]] a [[String]],
          TagUntagFD a ta, Typeable a, Data (HList a)
          ) =>
         HFWPresent ( TIP ta )
    where
        hfwPrint = putStrLn . showTIP
        hfwPrintTyped = putStrLn . showTIPTyped
        hfwPrintTypedTS ts t = putStrLn $ showTIPTypedTS ts t

instance ( HFoldr (Mapcar HPresentTIP) [[String]] a [[String]],
           HFoldr (Mapcar HPresentTIPTyped) [[String]] a [[String]],
           Typeable a, TagUntagFD a ta, 
           HFoldr (Mapcar (HComp HShow HUntag)) [String] ta [String],
           Data (HList a) ) =>
        FWPresent' HFWTIP ( TIP ta )
  where
    fwPresent' _      = buildTIPRep
    fwPresentTyped' _ = buildTIPRepTyped


-- Builds a full text representation of a string-list representation of a set of TIPs
buildTIPRep ::
     (Typeable a, TagUntagFD a ta,
      HFoldr (Mapcar HPresentTIP) [[String]] a [[String]]) =>
     TIP ta -> [String]
buildTIPRep rTup = present1LineValue ( listPresentTIP rTup ) ( map show $ tipInfo rTup )

buildTIPRepTyped ::
     (Data (HList a), TagUntagFD a ta,
      HFoldr (Mapcar HPresentTIPTyped) [[String]] a [[String]]) =>
     TIP ta -> [String]
buildTIPRepTyped rTup = present1LineValue ( listPresentTypedTIP rTup ) ( rhInfoToHdrTyped $ tipFullInfo rTup )

showTIP ::
     (Typeable a, TagUntagFD a ta,
      HFoldr (Mapcar HPresentTIP) [[String]] a [[String]]) =>
     TIP ta -> String
showTIP = intercalate "\n" . buildTIPRep

showTIPTyped ::
     (Data (HList a), TagUntagFD a ta,
      HFoldr (Mapcar HPresentTIPTyped) [[String]] a [[String]]) =>
     TIP ta -> String
showTIPTyped = intercalate "\n" . buildTIPRepTyped

showTIPTypedTS
  :: (Data (HList a), TagUntagFD a ta,
      HFoldr (Mapcar HPresentTIPTyped) [[String]] a [[String]],
      HListTypeSynonym ts) =>
     ts -> TIP ta -> String
showTIPTypedTS ts t =
  intercalate "\n" $
    present1LineValue ( listPresentTypedTIP t ) ( rhInfoToHdrTypedTS ts $ tipFullInfo t )


-- TODO: Is it possble to also have instances based on Data.Coercible?
data HPresentTIP = HPresentTIP
instance ([String] ~ stringL, FWPresent' (FWPPred (Unwrapped a)) (Unwrapped a), Wrapped a) => 
        ApplyAB HPresentTIP a stringL
    where
        applyAB _ x = fwPresent ( x ^. _Wrapped' )

data HPresentTIPTyped = HPresentTIPTyped
instance ([String] ~ stringL, FWPresent' (FWPPred (Unwrapped a)) (Unwrapped a), Wrapped a) => 
        ApplyAB HPresentTIPTyped a stringL
    where
        applyAB _ x = fwPresentTyped ( x ^. _Wrapped' )


-- TODO: Should be hMapOut instead? (This is relatively early code, lots of blind following of examples.)
listPresentTIP ::
     (Monad m, TagUntagFD l ta, HFoldr (Mapcar HPresentTIP) [m e] l [m e]) =>
     TIP ta -> [m e]
listPresentTIP = hMapM HPresentTIP . unTagTIP

listPresentTypedTIP ::
     (Monad m, TagUntagFD l ta, HFoldr (Mapcar HPresentTIPTyped) [m e] l [m e]) =>
     TIP ta -> [m e]
listPresentTypedTIP = hMapM HPresentTIPTyped . unTagTIP




-- == Presentation of a set of TIPs == --

-- | Builds a tabular represetation of a set of tips
buildSetTIPTab ::
     (Typeable ta, Typeable a, TagUntagFD a ta,
      HFoldr (Mapcar HPresentTIP) [[String]] a [[String]]) =>
     Set ( TIP ta ) -> [String]
buildSetTIPTab tips =
  presentNLineValue
    ( map listPresentTIP $ toList tips )
    ( map show $ tipSetHdr tips )

buildSetTIPTabTyped ::
     (Data (HList a), Typeable ta, TagUntagFD a ta,
      HFoldr (Mapcar HPresentTIPTyped) [[String]] a [[String]]) =>
     Set ( TIP ta ) -> [String]
buildSetTIPTabTyped tips =
  presentNLineValue
    ( map listPresentTypedTIP $ toList tips )
    ( rhInfoToHdrTyped $ tipSetHdrFull tips )

-- | Shows a set of tips in a tabular form
showTIPSetTab ::
     (Typeable ta, Typeable a, TagUntagFD a ta,
      HFoldr (Mapcar HPresentTIP) [[String]] a [[String]]) =>
     Set ( TIP ta ) -> String
showTIPSetTab = intercalate "\n" . buildSetTIPTab

-- | Shows a set of tips as a tabular form with type information
showTIPSetTabTyped ::
     (Data (HList a), Typeable ta, TagUntagFD a ta,
      HFoldr (Mapcar HPresentTIPTyped) [[String]] a [[String]]) =>
     Set ( TIP ta ) -> String
showTIPSetTabTyped = intercalate "\n" . buildSetTIPTabTyped

printTIPSet ::
     (Typeable ta, Typeable a, TagUntagFD a ta,
      HFoldr (Mapcar HPresentTIP) [[String]] a [[String]]) =>
     Set ( TIP ta ) -> IO ()
printTIPSet = putStrLn . showTIPSetTab

printTIPSetTyped ::
     (Data (HList a), Typeable ta, TagUntagFD a ta,
      HFoldr (Mapcar HPresentTIPTyped) [[String]] a [[String]]) =>
     Set ( TIP ta ) -> IO ()
printTIPSetTyped = putStrLn . showTIPSetTabTyped

printTIPSetTypedTS
  :: (Data (HList a), Typeable ta, TagUntagFD a ta,
      HFoldr (Mapcar HPresentTIPTyped) [[String]] a [[String]],
      HListTypeSynonym ts) =>
     ts -> Set (TIP ta) -> IO ()
printTIPSetTypedTS ts tipSet =
  putStrLn $ showTIPSetTabTypedTS ts tipSet
  where 
    showTIPSetTabTypedTS ts' tipSet' =
      intercalate "\n" $
      presentNLineValue
        ( map listPresentTypedTIP $ toList tipSet' )
        ( rhInfoToHdrTypedTS ts' $ tipSetHdrFull tipSet' )


instance (Typeable a, Typeable ta, TagUntagFD a ta,
      HFoldr (Mapcar HPresentTIP) [[String]] a [[String]],
      HFoldr (Mapcar HPresentTIPTyped) [[String]] a [[String]],
      Data (HList a)) =>
      HFWPresent ( Set ( TIP ta ) ) where
      hfwPrint = printTIPSet
      hfwPrintTyped = printTIPSetTyped
      hfwPrintTypedTS = printTIPSetTypedTS

instance ( HFoldr (Mapcar HPresentTIP) [[String]] a [[String]],
           HFoldr (Mapcar HPresentTIPTyped) [[String]] a [[String]],
           Typeable ta, Typeable a, TagUntagFD a ta, 
           HFoldr (Mapcar (HComp HShow HUntag)) [String] ta [String],
           Data (HList a) ) =>
        FWPresent' HFWTIPSet ( Set ( TIP ta ) )
  where
    fwPresent' _      = buildSetTIPTab
    fwPresentTyped' _ = buildSetTIPTabTyped



-- == Meta information == --

-- Converts an untipped TIP to a list of TypeReps.
hListInfo :: Typeable a => a -> [TypeRep]
hListInfo x = refineHListTypeRep trBody
    where
        trArgs = typeRepArgs $ typeOf x
        trBody = typeRepArgs $ head trArgs

-- [Foo,: Bar (: Baz [])] --> [Foo,Bar,Baz]
refineHListTypeRep :: [TypeRep] -> [TypeRep]
refineHListTypeRep []     = []
refineHListTypeRep [_]    = error "Not a HList TIP"
refineHListTypeRep (a:ax) = a : refineHListTypeRep ( typeRepArgs $ head ax )


type HdrInfo = [(TypeRep,[TypeRep])]

newtype TIPHdrInfo a = TIPHdrInfo HdrInfo

tipTypeFullInfo :: forall a. Data a => a -> HdrInfo
tipTypeFullInfo = reverse . tipTypeFullInfo'

tipTypeFullInfo' :: forall a. Data a => a -> HdrInfo
tipTypeFullInfo' t = tipTypeFullInfo'' t $ getAlgCtor t

tipTypeFullInfo'' :: forall a. Data a => a -> Constr -> HdrInfo
tipTypeFullInfo'' _ ctor = case go ctor of TIPHdrInfo reps -> reps
  where
  go :: Constr -> TIPHdrInfo a
  go = gunfold (\ (TIPHdrInfo infos :: TIPHdrInfo (b->r)) ->
                   let undefB = (undefined :: b) in
                   if null infos then TIPHdrInfo $ (typeOf undefB, attrTyInfo undefB) : infos
                                 else TIPHdrInfo $ tipTypeFullInfo' undefB ++ infos )
               (\_ -> TIPHdrInfo [])

-- Converts a TIP to a list of TypeReps.
tipInfo :: (Typeable a, TagUntagFD a ta) => TIP ta -> [TypeRep]
tipInfo = hListInfo . hUntagSelf . unTIP

tipFullInfo :: (Data (HList a), TagUntagFD a ta) => TIP ta -> HdrInfo
tipFullInfo = tipTypeFullInfo . hUntagSelf . unTIP

rhInfoToHdrTyped :: Show a => [(a, [TypeRep])] -> [String]
rhInfoToHdrTyped = map (\x -> show ( fst x ) ++ " :: " ++ intercalate ", " ( map showTR $ snd x ) )

rhInfoToHdrTypedTS
  :: (Show a, HListTypeSynonym ts) =>
     ts -> [(a, [TypeRep])] -> [String]
rhInfoToHdrTypedTS ts trList = map (\x -> show ( fst x ) ++ " :: " ++ intercalate ", " ( map ( showTRTS ts ) $ snd x ) ) trList

-- Meta information for sets of TIPs

tipSetToUndefHList :: (Typeable ta, TagUntagFD a ta) => Set (r ta) -> HList a
tipSetToUndefHList = undefined

tipSetHdr :: (Typeable a, Typeable ta, TagUntagFD a ta) => Set ( TIP ta ) -> [TypeRep]
tipSetHdr = hListInfo . tipSetToUndefHList

tipSetHdrFull :: (Data (HList a), Typeable ta, TagUntagFD a ta) =>
     Set ( TIP ta ) -> HdrInfo
tipSetHdrFull = tipTypeFullInfo . tipSetToUndefHList
