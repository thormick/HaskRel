{-|
Module      : FWTabulation
Description : Presentation of tables in a two-dimensional fixed-width font form.
Copyright   : © Thor Michael Støre, 2015
License     : GPL v2 without "any later version" clause
Maintainer  : thormichael át gmail døt com
Stability   : experimental

Presentation of tables in a two-dimensional fixed-width font form.
-}
module Database.HaskRel.FWTabulation (
  -- * Presentation functions
  present1LineValue, presentNLineValue,
  -- * Utility functions
  maxLen, padTranspose ) where

import Data.List ( intercalate )

-- | Given two lists of orderables, produces a list of equal length where each element is that which is the maximum of those elements that are in the same position in the two input lists.
maxLen :: Ord b => [b] -> [b] -> [b]
maxLen = zipWith max


buildOneColumn :: Int -> String -> String -> String
buildOneColumn hPad a str = str ++ a ++ concat ( replicate ( hPad - length a ) " " )

buildColumns :: [Int] -> [String] -> String
buildColumns [] [] = ""
buildColumns [p] [t] = buildOneColumn p t ""
buildColumns (p:ps) (t:ts) = buildColumns' (p:ps) (t:ts) ""

buildColumns [] [t] = buildOneColumn 0 t ""
buildColumns [] (t:ts) = buildColumns' [] (t:ts) ""

buildColumns [p] [] = buildOneColumn p "" "Internal Error: "
buildColumns (p:ps) [] = buildColumns' (p:ps) [] ""

buildColumns' :: [Int] -> [String] -> String -> String
buildColumns' [] [] str = str
buildColumns' [p] [t] str = buildOneColumn p t str
buildColumns' (p:ps) (t:ts) str = buildColumns' ps ts $ buildOneColumn p t str ++ " │ "

-- These error messages stem from before HList was used and a less disciplined form was used instead, they should be impossible to trigger today barring the introduction of bugs.
-- These are pure presentation functions, so it's okay to just inform of the error rather than programmatically signaling it
buildColumns' [] [t] str = str ++ "Internal Error: " ++ t
buildColumns' [] (t:ts) str = str ++ "Internal Error: " ++ intercalate " │ " (t:ts)

buildColumns' [p] [] str = str ++ "Internal Error: " ++ show p
buildColumns' (p:ps) [] str = str ++ "Internal Error: " ++ intercalate " │ " ( map show $ p:ps )


-- | Gets the maximum length of each column of a value consisting of a header and a single line
colWidths ::
  (Foldable t, Foldable t1) =>
  [[t1 a1]] -> [t a] -> [Int]
colWidths l hdr = foldl1 maxLen [ map length hdr, allColWidths l ]

-- | Gets the width of the columns of a value in when presented in a columnar format.
allColWidths :: Foldable t => [[t a]] -> [Int]
allColWidths = map $ maximum . map length

-- | Gets the maximum length of each column of a value consisting of a header and zero or more lines
nColWidths :: (Foldable t, Foldable t1) => [[[t1 a1]]] -> [t a] -> [Int]
nColWidths ll hdr = foldl1 maxLen $ map length hdr : mapListLen ll

mapListLen :: Foldable t => [[[t a]]] -> [[Int]]
mapListLen = map allColWidths

-- See also: http://en.wikipedia.org/wiki/Box-drawing_character
-- | Builds a one-line representation of a value, plus header
present1LineValue :: [[String]] -> [String] -> [String]
present1LineValue strRep header =
  let hPad = colWidths strRep header
   in
  [ "┌─" ++ hPadTable hPad "─" "─┬─" ++ "─┐",
    "│ " ++ buildColumns hPad header ++ " │",
    "├─" ++ hPadTable hPad "─" "─┼─" ++ "─┤"]
 ++ buildRow hPad strRep ++
  [ "└─" ++ hPadTable hPad "─" "─┴─" ++ "─┘"]

-- | Builds a multi-line representation of a value, plus header
presentNLineValue :: [[[String]]] -> [String] -> [String]
presentNLineValue strRepList' hdr =
  let
    hPad = nColWidths strRepList' hdr
  in
    [ "┌─" ++ hPadTable hPad "─" "─┬─" ++ "─┐",
      "│ " ++ buildColumns hPad hdr ++ " │",
-- Classic double-line for candidate key, doesn't always display correctly:
      "╞═" ++ hPadTable hPad "═" "═╪═" ++ "═╡" ]
-- Strong line for candidate key, also doesn't always display correctly:
--      "┝━" ++ hPadTable hPad "━" "━┿━" ++ "━┥" ]
-- No indication of candidate key, seems to work in more cases:
--      "├─" ++ hPadTable hPad "─" "─┼─" ++ "─┤"]
   ++ foldr (\a b -> buildRow hPad a ++ b ) [] strRepList' ++ 
    [ "└─" ++ hPadTable hPad "─" "─┴─" ++ "─┘" ]

buildRow :: [Int] -> [[String]] -> [String]
buildRow hPad strRep = buildRow' hPad $ padTranspose strRep

buildRow' :: [Int] -> [[String]] -> [String]
buildRow' _ [] = ["│  │"]
buildRow' hPad [strRep] = ["│ " ++ buildColumns hPad strRep ++ " │"]
buildRow' hPad (strRep:strRepX) = ( "│ " ++ buildColumns hPad strRep ++ " │" ) : buildRow' hPad strRepX


-- | Transposes a list of lists of lists, padding the lists of the second dimension with empty lists if they are shorter than the other rows.
padTranspose :: [[[t]]] -> [[[t]]]
padTranspose x = padTranspose' x ( maximum ( map length x ) - 1 )

padTranspose' :: [[[t]]] -> Int -> [[[t]]]
padTranspose' [] _ = []
padTranspose' ([] : xss) l = padTranspose' xss ( l - 1 )
padTranspose' ((x:xs) : xss) l = (x : map next xss) : padTranspose' (rPad l xs : map rest xss) ( l - 1 )

--next :: (Monoid a) => [a] -> a
-- | Gives the head of the argument, or the empty list if the argument is empty.
next :: [[t]] -> [t]
next [] = []
next xs = head xs

--rest :: (Monoid a) => [a] -> [a]
-- | Gives the tail of the argument, or the empty list if the argument is empty.
rest :: [t] -> [t]
rest [] = []
rest xs = tail xs

rPad :: Int -> [[t]] -> [[t]]
rPad m xs = xs ++ replicate ( m - length xs ) []

hPadColumn :: Int -> [a] -> [a]
hPadColumn x fillChar = concat ( replicate x fillChar )

hPadTable :: [Int] -> String -> String -> String
hPadTable [] _ _ = ""
hPadTable [x] fillChar _ = hPadColumn x fillChar
hPadTable (x:xs) fillChar divChar = hPadColumn x fillChar ++ divChar ++ hPadTable xs fillChar divChar
