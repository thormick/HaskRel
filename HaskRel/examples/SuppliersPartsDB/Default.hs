{-# LANGUAGE DataKinds #-}

{-|
Default contents for the SuppliersPartsDB.
-}
module SuppliersPartsDB.Default where

import Database.HaskRel.Relational.Definition ( Relation, relation, relation', rTuple )

import SuppliersPartsDB.Definition

import Data.HList.CommonMain ((.=.))

{-
Tutorial D:

RELATION
{ TUPLE { SNO ‘S1’ , SNAME ‘Smith’ , STATUS 20 , CITY ‘London’ } ,
  TUPLE { SNO ‘S2’ , SNAME ‘Jones’ , STATUS 10 , CITY ‘Paris’ } ,
  TUPLE { SNO ‘S3’ , SNAME ‘Blake’ , STATUS 30 , CITY ‘Paris’ } ,
  TUPLE { SNO ‘S4’ , SNAME ‘Clark’ , STATUS 20 , CITY ‘London’ } ,
  TUPLE { SNO ‘S5’ , SNAME ‘Adams’ , STATUS 30 , CITY ‘Athens’ } }


In Tutorial D attribute identifiers can both be used without arguments (within a query to denote a attribute of a relation in scope), or with an argument as above, as a attribute constructor. The same can be achieved in Haskell in various ways. One way is defining all pertinent functions such that they allow both a label and a constructor without an argument, and then only defining such constructors:

status a = (Label::Label "status") .=. (a::Integer)

Which would allow:

rTuple ( sno "S1" , sName "Smith" , status 20 , city "London" )

While still letting one use sno, sName, etc as attribute identifiers in functions. However, doing this consistently requires some rather convoluted type-level acrobatics that causes exponential growth in function definition complexity. As such only labels are defined in these examples, which one must use .=. to construct into the tagged values that are directly employed by HaskRel as relational attributes.

An alternative would be https://hackage.haskell.org/package/HList/docs/Data-HList-Labelable.html, though HaskRel records don't build on lens and can't accomodate that approach as-is.
-}


s' =
    relation
    [ rTuple ( sno .=. "S1" , sName .=. "Smith" , status .=. 20 , city .=. "London" ) ,
      rTuple ( sno .=. "S2" , sName .=. "Jones" , status .=. 10 , city .=. "Paris" ) ,
      rTuple ( sno .=. "S3" , sName .=. "Blake" , status .=. 30 , city .=. "Paris" ) ,
      rTuple ( sno .=. "S4" , sName .=. "Clark" , status .=. 20 , city .=. "London" ) ,
      rTuple ( sno .=. "S5" , sName .=. "Adams" , status .=. 30 , city .=. "Athens" ) ]
        :: Relation '[SNO, SName, Status, City]

p' =
    relation
    [ rTuple ( pno .=. "P1" , pName .=. "Nut" , color .=. "Red" , weight .=. 12.0 , city .=. "London" ) ,
      rTuple ( pno .=. "P2" , pName .=. "Bolt" , color .=. "Green" , weight .=. 17.0 , city .=. "Paris" ) ,
      rTuple ( pno .=. "P3" , pName .=. "Screw" , color .=. "Blue" , weight .=. 17.0 , city .=. "Oslo" ) ,
      rTuple ( pno .=. "P4" , pName .=. "Screw" , color .=. "Red" , weight .=. 14.0 , city .=. "London" ) ,
      rTuple ( pno .=. "P5" , pName .=. "Cam" , color .=. "Blue" , weight .=. 12.0 , city .=. "Paris" ) ,
      rTuple ( pno .=. "P6" , pName .=. "Cog" , color .=. "Red" , weight .=. 19.0 , city .=. "London" ) ]
        :: Relation '[PNO, PName, Color, Weight, City]

-- Let's say that I'm tired of repeating the attribute names; syntactic sugar to
-- the rescue, note the use of relation' instead of relation, and that the type
-- annotation becomes mandatory:
sp' =
    relation'
    [ ("S1", "P1", 300), 
      ("S1", "P2", 200), 
      ("S1", "P3", 400), 
      ("S1", "P4", 200), 
      ("S1", "P5", 100), 
      ("S1", "P6", 100), 
      ("S2", "P1", 300), 
      ("S2", "P2", 400), 
      ("S3", "P2", 200), 
      ("S4", "P2", 200), 
      ("S4", "P4", 300), 
      ("S4", "P5", 400) ] :: Relation '[SNO, PNO, QTY]

{-
Also note how the attribute names above, SNO etc, are declared type synonyms in Definition.hs, and not ad-hoc. An equivalent ad-hoc definition would be:

Relation '[Tagged "sno" String, Tagged "pno" String, Tagged "qty" Integer]


An alternative approach is to define constructors for each label, which would also make it more type-safe:
sno' a = (Label :: Label "sno") .=. a :: SNO

There may be a way to build the functions of the relational algebra such that they accept both "Label l" and "v -> Tagged l v" types, which would make it possible that instead of label constants one could just define constructors and used those against the functions of the relational algebra, but an investigation into that revealed no attempt that didn't have an unacceptable drawback.
-}
