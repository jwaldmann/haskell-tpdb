module TPDB.Pretty 

( Doc, SimpleDoc
, render, renderCompact, displayIO
, Pretty (..)
, fsep, sep, hsep, vsep, vcat, hcat
, parens, brackets, angles, braces, enclose
, punctuate, comma, nest
, empty, text
, (<>), (<+>), ($$)
)

where

import Text.PrettyPrint.Leijen.Text 
    hiding ( text, (<+>), vcat, hcat, vsep, hsep, sep, parens )
import qualified Text.PrettyPrint.Leijen.Text 
import Data.String ( fromString )

-- class Pretty a where pretty :: a -> Doc

($$) = (<$$>)
x <+> y = x Text.PrettyPrint.Leijen.Text.<+> align (group y)
vcat = align . Text.PrettyPrint.Leijen.Text.vcat . map group
hcat = align . Text.PrettyPrint.Leijen.Text.hcat . map group
vsep = align . Text.PrettyPrint.Leijen.Text.vsep . map group
hsep = align . Text.PrettyPrint.Leijen.Text.hsep . map group
fsep = align . Text.PrettyPrint.Leijen.Text.fillSep . map group
sep = align . Text.PrettyPrint.Leijen.Text.sep . map group
parens = Text.PrettyPrint.Leijen.Text.parens . group
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

instance ( Pretty a, Pretty b ) => Pretty (Either a b) where
    pretty (Left x) = text "Left" <+> parens (pretty x)
    pretty (Right x) = text "Right" <+> parens (pretty x)
