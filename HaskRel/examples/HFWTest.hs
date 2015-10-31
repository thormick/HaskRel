{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}



module HFWTest where

import Control.Lens.Wrapped ( Wrapped, Unwrapped, _Wrapped', _Unwrapped' )
import Control.Lens ( (^.) )
import Control.Lens.TH ( makeWrapped )

import Data.Typeable
import Data.Data

import Data.HList.CommonMain

import Database.HaskRel.FWTabulation
import Database.HaskRel.HFWTabulation
import Database.HaskRel.TIPFWTabulation

import Database.HaskRel.Relational.TIP.Definition





sno = Label :: Label "sno"
status = Label :: Label "status"
sno_status = Label :: Label "sno_status"




newtype SNO    = SNO    String   deriving ( Eq, Show, Read, Ord, Typeable, Data )
newtype SName  = SName  String   deriving ( Eq, Show, Read, Ord, Typeable, Data )
newtype Status = Status Integer  deriving ( Eq, Show, Read, Ord, Typeable, Data )
newtype City   = City   String   deriving ( Eq, Show, Read, Ord, Typeable, Data )

newtype PNO    = PNO    String   deriving ( Eq, Show, Read, Ord, Typeable, Data )
newtype PName  = PName  String   deriving ( Eq, Show, Read, Ord, Typeable, Data )
newtype Color  = Color  String   deriving ( Eq, Show, Read, Ord, Typeable, Data )
newtype Weight = Weight Rational deriving ( Eq, Show, Read, Ord, Typeable, Data )

newtype QTY    = QTY    Integer  deriving ( Eq, Show, Read, Ord, Typeable, Data )

makeWrapped ''SNO
makeWrapped ''SName
makeWrapped ''Status
makeWrapped ''City

makeWrapped ''PNO
makeWrapped ''PName
makeWrapped ''Color
makeWrapped ''Weight

makeWrapped ''QTY

{-
newtype PQ = PQ ( Relation '[PNO, QTY] ) deriving ( Eq, Show, Ord, Typeable, Data )
instance {-# OVERLAPPING #-} Data ( TIP '[Tagged PNO PNO, Tagged QTY QTY] ) where
    gunfold = undefined
    toConstr = undefined
    dataTypeOf = undefined

makeWrapped ''PQ
-}
