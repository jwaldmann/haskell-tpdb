module TPDB.Pretty 

( Doc, SimpleDoc
, render, renderCompact, displayIO
, Pretty (..)
, fsep , hsep, vsep, vcat, hcat
, parens, brackets, angles, braces, enclose
, punctuate, comma, nest
, empty, text
, (<>), (<+>), ($$)
)

where

import Text.PrettyPrint.Leijen.Text hiding ( text )
import Data.String ( fromString )

-- class Pretty a where pretty :: a -> Doc

fsep = fillSep
($$) = (<$$>)

render :: Doc -> String
render = show

text :: String -> Doc
text = fromString

{-

instance Pretty Int where pretty = text . show

instance ( Pretty a, Pretty b ) => Pretty (a,b) where
    pretty (x,y) = parens $ fsep $ punctuate comma [ pretty x, pretty y ]

instance ( Pretty a, Pretty b, Pretty c ) => Pretty (a,b,c) where
    pretty (x,y,z) = parens $ fsep $ punctuate comma [ pretty x, pretty y, pretty z ]
-}

instance ( Pretty a, Pretty b, Pretty c, Pretty d ) => Pretty (a,b,c,d) where
    pretty (x,y,z,u) = parens $ fsep $ punctuate comma [ pretty x, pretty y, pretty z, pretty u ]

{-
instance Pretty a => Pretty [a]  where
    pretty xs = brackets $ fsep $ punctuate comma $ map pretty xs

instance Pretty a => Pretty (Maybe a) where
    pretty m = case m of
        Nothing -> text "Nothing"
        Just x -> text "Just" <+> pretty x -- FIXME: parens missing
-}