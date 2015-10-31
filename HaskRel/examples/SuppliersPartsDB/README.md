The relvars of SuppliersPartsDB, with types:

    >>> pt$ s
    ┌───────────────┬─────────────────┬───────────────────┬────────────────┐
    │ sno :: String │ sName :: String │ status :: Integer │ city :: String │
    ╞═══════════════╪═════════════════╪═══════════════════╪════════════════╡
    │ S1            │ Smith           │ 20                │ London         │
    │ S2            │ Jones           │ 10                │ Paris          │
    │ S3            │ Blake           │ 30                │ Paris          │
    │ S4            │ Clark           │ 20                │ London         │
    │ S5            │ Adams           │ 30                │ Athens         │
    └───────────────┴─────────────────┴───────────────────┴────────────────┘
    >>> pt$ p
    ┌───────────────┬─────────────────┬─────────────────┬─────────────────────────┬────────────────┐
    │ pno :: String │ pName :: String │ color :: String │ weight :: Ratio Integer │ city :: String │
    ╞═══════════════╪═════════════════╪═════════════════╪═════════════════════════╪════════════════╡
    │ P1            │ Nut             │ Red             │ 12 % 1                  │ London         │
    │ P2            │ Bolt            │ Green           │ 17 % 1                  │ Paris          │
    │ P3            │ Screw           │ Blue            │ 17 % 1                  │ Oslo           │
    │ P4            │ Screw           │ Red             │ 14 % 1                  │ London         │
    │ P5            │ Cam             │ Blue            │ 12 % 1                  │ Paris          │
    │ P6            │ Cog             │ Red             │ 19 % 1                  │ London         │
    └───────────────┴─────────────────┴─────────────────┴─────────────────────────┴────────────────┘
    >>> pt$ sp
    ┌───────────────┬───────────────┬────────────────┐
    │ sno :: String │ pno :: String │ qty :: Integer │
    ╞═══════════════╪═══════════════╪════════════════╡
    │ S1            │ P1            │ 300            │
    │ S1            │ P2            │ 200            │
    │ S1            │ P3            │ 400            │
    │ S1            │ P4            │ 200            │
    │ S1            │ P5            │ 100            │
    │ S1            │ P6            │ 100            │
    │ S2            │ P1            │ 300            │
    │ S2            │ P2            │ 400            │
    │ S3            │ P2            │ 200            │
    │ S4            │ P2            │ 200            │
    │ S4            │ P4            │ 300            │
    │ S4            │ P5            │ 400            │
    └───────────────┴───────────────┴────────────────┘

(Note that certain combinations of operating systems and browsers don't seem to want to display the double line of the tables correctly. On OS X 10.8 Google Chrome doesn't, whereas Firefox seems to do just fine. See also the [Wikipedia entry on box-drawing characters](http://en.wikipedia.org/wiki/Box-drawing_character#Examples).)

Relation values named s', p' and sp' are also defined, of the same values as above.

    >>> :t s
    s :: Relvar '[SNO, SName, Status, City]
    >>> :t s'
    s' :: Relation '[SNO, SName, Status, City]
    >>> :i SNO
    type SNO = Attr "sno" String
      	-- Defined at SuppliersPartsDB/Definition.hs:16:1
    >>> s `rEq` s'
    True

<tt>pt$ s'</tt>, <tt>pt$ p'</tt> and <tt>pt$ sp'</tt> will give the same result as above.
