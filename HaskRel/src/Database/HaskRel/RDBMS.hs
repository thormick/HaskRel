
{-|
Module      : Database.HaskRel.RDBMS
Description : A re-export of the modules that form the HList record based HaskRel library
Copyright   : © Thor Michael Støre, 2015
License     : GPL v2 without "any later version" clause
Maintainer  : thormichael át gmail døt com
Stability   : experimental

Exports the pertinent parts of HaskRel building on HList records. This gives most features of the relational algebra, relation variable support, HList CommonMain, as well as certain non-relational features such as ordering.
-}

-- TODO: Should call this something else than RDBMS seeing as it's so far just a DBMS + relational algebra
module Database.HaskRel.RDBMS (
  module Data.HList.CommonMain,
  -- * The relational model of database management: A subset thereof
  module Database.HaskRel.Relational.Definition,
  Relvar ( Relvar, relvarPath ), relvarType, readRelvar,
  module Database.HaskRel.Relational.Expression,
  module Database.HaskRel.Relational.Unicode,
  -- * Non-relational features
  module Database.HaskRel.Order,
  module Database.HaskRel.Support ) where

-- TODO: Only import relevant parts of HList. Variant, TIPs and TICs aren't required.
import Data.HList.CommonMain
import Data.Tagged (Tagged)

-- The relational model, and features defined together with it
import Database.HaskRel.Relational.Definition
import Database.HaskRel.Relational.Variable ( Relvar (Relvar), relvarPath, relvarType, readRelvar )
import Database.HaskRel.Relational.Expression
import Database.HaskRel.Relational.Unicode

-- Features not of the relational model
import Database.HaskRel.Order
import Database.HaskRel.Support

