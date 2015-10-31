{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module AlgebraExample where

--import Database.HaskRelTIP.Relational.Assignment ( assign )
import Database.HaskRelTIP.Relational.Definition
  ( Relation, relation, uniRelation, RTuple, RHdr, rTuple, uniRTuple, pt, rPrint )
import Database.HaskRel.Support

import Database.HaskRelTIP.Relational.Variable ( readRelvar )

-- import Database.HaskRelTIP.Relational.Expression
import Database.HaskRelTIP.Relational.Assignment ( update )
import Database.HaskRelTIP.Relational.Algebra

import SuppliersPartsDB.Definition
import SuppliersPartsDB.Default

import ExampleTypes

-- To make a few type signatures prettier
import Data.Set ( Set )

-- To use HList constructors directly, instead of "relation"/"relation'"
import Data.Set ( fromList )
import Data.HList.CommonMain

r1 = relation [(SNO "S2",PNO "P1"),
               (SNO "S2",PNO "P2"),
               (SNO "S3",PNO "P2"),
               (SNO "S4",PNO "P2"),
               (SNO "S4",PNO "P4"),
               (SNO "S4",PNO "P5")]

r4 = relation [(SNO "S2", PNORel $ uniRelation [(PNO "P1"), (PNO "P2")]),
               (SNO "S3", PNORel $ uniRelation [(PNO "P2")]),
               (SNO "S4", PNORel $ uniRelation [(PNO "P2"),(PNO "P4"),(PNO "P5")])]
