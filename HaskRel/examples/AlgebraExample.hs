{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
--{-# LANGUAGE ViewPatterns #-}

module AlgebraExample where

import Data.HList.CommonMain

import Control.Lens ( (^.), from, to )

-- To make a few type signatures prettier
import Data.Set ( Set )

-- To use HList constructors directly, instead of "relation"/"relation'"
import Data.Set ( fromList )

--

import Database.HaskRel.Support
import Database.HaskRel.Relational.Definition
import Database.HaskRel.Relational.Algebra


import SuppliersPartsDB.Definition
import SuppliersPartsDB.Default

--
import qualified Data.Set ( map )

{-

let fooAgg = agg sno

let sumqty = sum . agg qty


λ> :t agg
agg :: (Foldable t, HasField l a a1) => Label l -> t a -> [a1]
λ> :t (\a -> agg (\ [pun|sno pno|] -> sno ) a )
nope

aggP: aggregate-on-pun, to give multiple variables based on aggregated attributes...

λ> :t  [pun|sno pno|]
[pun|sno pno|]
  :: HExtendR
       (Tagged "sno" (Label "sno")) (Record '[Tagged "pno" (Label "pno")])

-- Relation '[Tagged "sno" String, Tagged "qty" Integer] -> Record '[Tagged "qty" Integer]


:t groupR2 sp' (undefined :: Relation '[Tagged "pno" String, Tagged "qty" Integer] -> Record '[Tagged "qty" Integer] )


groupR2 sp' (\(r:: Relation '[Tagged "pno" String, Tagged "qty" Integer] -> Record '[Tagged "qty" Integer]) -> ( Label :: Label "qty" ) .=. agg qty r .*. emptyRecord)


(undefined :: Relation '[Tagged "pno" String, Tagged "qty" Integer] -> Record '[Tagged "qty" Integer] )

-}
-- group rel attsIn = extendByImage rel $ rel `projectAllBut` attsIn

-- sno .=. "qwer" .*. hDeleteAtLabel (Label::Label "sno") (sno .=. "asf" .*. emptyRecord)

extendOrUpdate ::
     (HExtend e (r v'), HDeleteAtLabel r l v v') =>
     r v -> e -> Label l -> HExtendR e (r v')
extendOrUpdate r v l = v .*. hDeleteAtLabel l r

extendOrUpdate' :: forall l e r v v'.
    (HDeleteAtLabel r l v v', HExtend (Tagged l e) (r v')) =>
    r v -> Tagged l e -> HExtendR (Tagged l e) (r v')
extendOrUpdate' r v = v .*. hDeleteAtLabel (Label::Label l) r

-- * Different ways to represent relations

{-
The standard representation, patterned after Tutorial D. Dependent upon defining
data construction functions, or using the HList .=. operator. Uses the rTuple
function and explicit construction of the fields.
-}
snoStatusR1 = relation [rTuple (sno .=. "S1", status .=. 40),
                        rTuple (sno .=. "S2", status .=. 30)]

{-
The inductive representation, where the body is made more consise by only
specifying the values, and relying on type induction to define the type and the
labels of the elements (attribute type and attribute name in the relational
model). This also frees one from dependence upon defining data construction
functions. It holds that snoStatusR1 == snoStatusR2
-}
snoStatusR2 = relation' [("S1", 40),
                         ("S2", 30)]
              :: Relation '[SNO, Status]
{- Remembering how SNO and Status are type synonyms, the last line could just as well be:
              :: Relation '[Attr "sno" String, Attr "status" Int]

Which is the form one must employ when building relation values of ad-hoc types.
-}

{-
If one wants to use the relation/rTuple functions, but in a completely ad-hoc
manner, it'll have to look as follows.
-}
snoStatusR3 = relation [
  rTuple ((Label :: Label "sno") .=. "S1", (Label :: Label "status") .=. (40::Int)),
  rTuple ((Label :: Label "sno") .=. "S2", (Label :: Label "status") .=. 30)]

{-
While the standard Haskell and HList approach will look like this:
-}

snoStatusR4 = Data.Set.fromList [
  (Label :: Label "sno") .=. "S1" .*. (Label :: Label "status") .=. (40::Int) .*. emptyRecord,
  (Label :: Label "sno") .=. "S2" .*. (Label :: Label "status") .=. 30 .*. emptyRecord]


-- Single attribute extension:
-- (\ ((\[pun|sno status|] -> (sno++"-foo", status+5)) -> sno) -> (Label::Label "sno") .=. sno ) (sno "input" .*. status (5::Integer) .*. emptyRecord)

type GMWT   = Attr "gmwt"   Rational
gmwt = Label::Label "gmwt"

type SCity   = Attr "sCity"   String
sCity = Label :: Label "sCity"

type PQ = Attr "pq" (Relation '[PNO,QTY])
pq = Label :: Label "pq"
