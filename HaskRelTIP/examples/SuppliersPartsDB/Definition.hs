{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- | Data definition for TIP based suppliers and parts database. -}

module SuppliersPartsDB.Definition
    ( -- | Attribute definitions
      SNO(SNO), SName(SName), Status(Status), City(City), PNO(PNO), PName(PName),
      Color(Color), Weight(Weight), QTY(QTY),
      -- | Relvar definitions
      s, sp, p,
      -- | Relation types
      S, SP, P,
      -- | Filesystem path
      dbPath ) where

import Data.Data (Data)
import Data.Typeable (Typeable)
import Control.Lens.TH ( makeWrapped )

import Data.HList.CommonMain ( TagR )

import Database.HaskRelTIP.Relational.Definition ( Relation, Relation' )
import Database.HaskRelTIP.Relational.Variable ( Relvar (Relvar) )
import Database.HaskRelTIP.Relational.Expression ()


dbPath = "SuppliersPartsDB/"

-- Attributes

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


{- TODO: Relvar type should be a Relation, like its argument, or just '[], not a TagR '[...], and the following types should be Relation or plain '[] instead of TagR. -}

type S  = TagR '[SNO, SName, Status, City]
type SP = TagR '[SNO, PNO, QTY]
type P  = TagR '[PNO, PName, Color, Weight, City]

s  :: Relvar S
s   = Relvar ( dbPath ++ "S.rv" )
sp :: Relvar SP
sp  = Relvar ( dbPath ++ "SP.rv" )
p  :: Relvar P
p   = Relvar ( dbPath ++ "P.rv" )

