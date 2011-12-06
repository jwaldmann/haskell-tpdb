module TPDB.Pretty where

import Text.PrettyPrint.HughesPJ

class Pretty a where pretty :: a -> Doc

instance Pretty Int where pretty = text . show

instance ( Pretty a, Pretty b ) => Pretty (a,b) where
    pretty (x,y) = parens $ fsep $ punctuate comma [ pretty x, pretty y ]

instance Pretty a => Pretty [a]  where
    pretty xs = brackets $ fsep $ punctuate comma $ map pretty xs