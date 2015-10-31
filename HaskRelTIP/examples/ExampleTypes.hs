{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- For RVAs:
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Additional types for examples, principally for the suppliers-parts database. -}

module ExampleTypes
{-    ( -- * Generic attribute definitions
      XString(XString), XInteger(XInteger), XRational(XRational),
      -- * Attributes for SuppliersParts examples
      SA(SA), SB(SB), SCity(SCity), PCity(PCity), GMWT(GMWT), SN(SN), PN(PN),
      -- | RVA types
      PQ(PQ) ) -} where

import Data.Data (Data, gunfold, toConstr, dataTypeOf)
import Data.Typeable (Typeable)
import Control.Lens.TH ( makeWrapped )

import Data.HList (TIP, Tagged)

import SuppliersPartsDB.Definition

import Database.HaskRelTIP.Relational.Definition ( RTuple, Relation )


-- Additional attribute types for eXamples:
newtype XString = XString String deriving ( Eq, Show, Read, Ord, Typeable, Data )
newtype XInteger = XInteger Integer deriving ( Eq, Show, Read, Ord, Typeable, Data )
newtype XRational = XRational Rational deriving ( Eq, Show, Read, Ord, Typeable, Data )

makeWrapped ''XString
makeWrapped ''XInteger
makeWrapped ''XRational


newtype SA = SA String deriving ( Eq, Show, Read, Ord, Typeable, Data )
newtype SB = SB String deriving ( Eq, Show, Read, Ord, Typeable, Data )

newtype SN = SN String deriving ( Eq, Show, Read, Ord, Typeable, Data )
newtype PN = PN String deriving ( Eq, Show, Read, Ord, Typeable, Data )

newtype SCity = SCity String deriving ( Eq, Show, Read, Ord, Typeable, Data )
newtype PCity = PCity String deriving ( Eq, Show, Read, Ord, Typeable, Data )

newtype GMWT = GMWT Rational deriving ( Eq, Show, Read, Ord, Typeable, Data )

newtype NC = NC String deriving ( Eq, Show, Read, Ord, Typeable, Data )
newtype NW = NW Rational deriving ( Eq, Show, Read, Ord, Typeable, Data )

newtype PQ = PQ ( Relation '[PNO, QTY] ) deriving ( Eq, Show, Ord, Typeable, Data )
instance {-# OVERLAPPING #-} Data ( TIP '[Tagged PNO PNO, Tagged QTY QTY] ) where
    gunfold = undefined
    toConstr = undefined
    dataTypeOf = undefined

newtype PNORel = PNORel ( Relation '[PNO] ) deriving ( Eq, Show, Ord, Typeable, Data )
instance {-# OVERLAPPING #-} Data ( TIP '[Tagged PNO PNO] ) where
    gunfold = undefined
    toConstr = undefined
    dataTypeOf = undefined

newtype PQTup = PQTup ( RTuple '[PNO, QTY] ) deriving ( Eq, Show, Ord, Typeable, Data )
newtype PNOTup = PNOTup ( RTuple '[PNO] ) deriving ( Eq, Show, Ord, Typeable, Data )
                                    
newtype X = X ( Relation '[PNO] ) deriving ( Eq, Show, Ord, Typeable, Data )
-- Already have a Data instance for TIP '[Tagged PNO PNO]
                
newtype Y = Y ( Relation '[PNO] ) deriving ( Eq, Show, Ord, Typeable, Data )
-- Already have a Data instance for TIP '[Tagged PNO PNO]


makeWrapped ''SA
makeWrapped ''SB

makeWrapped ''SN
makeWrapped ''PN

makeWrapped ''SCity
makeWrapped ''PCity

makeWrapped ''GMWT

makeWrapped ''NC
makeWrapped ''NW

makeWrapped ''PQ
makeWrapped ''PNORel

makeWrapped ''PQTup
makeWrapped ''PNOTup

makeWrapped ''X
makeWrapped ''Y
