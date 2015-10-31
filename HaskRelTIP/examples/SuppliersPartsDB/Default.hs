
{-|
Default contents for the SuppliersPartsDB.
-}
module SuppliersPartsDB.Default where

import Database.HaskRelTIP.Relational.Definition ( Relation, Relation', relation, relation' )

import SuppliersPartsDB.Definition 

{-
Tutorial D:

RELATION
{ TUPLE { SNO ‘S1’ , SNAME ‘Smith’ , STATUS 20 , CITY ‘London’ } ,
  TUPLE { SNO ‘S2’ , SNAME ‘Jones’ , STATUS 10 , CITY ‘Paris’ } ,
  TUPLE { SNO ‘S3’ , SNAME ‘Blake’ , STATUS 30 , CITY ‘Paris’ } ,
  TUPLE { SNO ‘S4’ , SNAME ‘Clark’ , STATUS 20 , CITY ‘London’ } ,
  TUPLE { SNO ‘S5’ , SNAME ‘Adams’ , STATUS 30 , CITY ‘Athens’ } }
-}

s' =
    relation
    [ ( SNO "S1" , SName "Smith" , Status 20 , City "London" ) ,
      ( SNO "S2" , SName "Jones" , Status 10 , City "Paris" ) ,
      ( SNO "S3" , SName "Blake" , Status 30 , City "Paris" ) ,
      ( SNO "S4" , SName "Clark" , Status 20 , City "London" ) ,
      ( SNO "S5" , SName "Adams" , Status 30 , City "Athens" ) ]

p' =
    relation
    [ ( PNO "P1" , PName "Nut" , Color "Red" , Weight 12.0 , City "London" ) ,
      ( PNO "P2" , PName "Bolt" , Color "Green" , Weight 17.0 , City "Paris" ) ,
      ( PNO "P3" , PName "Screw" , Color "Blue" , Weight 17.0 , City "Oslo" ) ,
      ( PNO "P4" , PName "Screw" , Color "Red" , Weight 14.0 , City "London" ) ,
      ( PNO "P5" , PName "Cam" , Color "Blue" , Weight 12.0 , City "Paris" ) ,
      ( PNO "P6" , PName "Cog" , Color "Red" , Weight 19.0 , City "London" ) ]

-- Okay, I'm tired of repeating the attribute names; syntactic sugar to the
-- rescue, note the use of relation' instead of relation, and of the type
-- annotation:
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
      ("S4", "P5", 400) ] :: Relation' SP

