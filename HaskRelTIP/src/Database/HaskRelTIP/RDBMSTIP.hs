
{-|
Module      : Database.HaskRelTIP.RDBMSTIP
Description : A re-export of the modules that form the HList TIP based HaskRel library
Copyright   : © Thor Michael Støre, 2015
License     : GPL v2 without "any later version" clause
Maintainer  : thormichael át gmail døt com
Stability   : experimental

Exports the pertinent parts of HaskRel building on HList TIPs. This gives most features of the relational algebra, relation variable support, and certain non-relational features such as ordering.
-}

module Database.HaskRelTIP.RDBMSTIP (
  -- | The linked modules are reexported in their entirety, while the `Relvar` data type is documented separately
  module Database.HaskRelTIP.Relational.Definition,
  module Database.HaskRelTIP.Relational.Expression,
  module Database.HaskRelTIP.Relational.Assignment,
  module Database.HaskRelTIP.Relational.Unicode,
  Relvar ( Relvar, relvarPath ), relvarType, readRelvar,
  module Database.HaskRel.Order ) where

-- The relational model, and features defined together with it
import Database.HaskRelTIP.Relational.Definition
import Database.HaskRelTIP.Relational.Variable ( Relvar (Relvar), relvarPath, relvarType, readRelvar )
import Database.HaskRelTIP.Relational.Expression
import Database.HaskRelTIP.Relational.Assignment hiding ( assign )
import Database.HaskRelTIP.Relational.Unicode

-- Features not of the relational model
import Database.HaskRel.Order
