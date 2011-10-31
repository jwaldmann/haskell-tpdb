-- | the "old" TPDB format 
-- cf. http://www.lri.fr/~marche/tpdb/format.html

module TPDB.Plain.Write where

import TPDB.Data

import Text.PrettyPrint.HughesPJ

class Pretty a where pretty :: a -> Doc

instance Pretty Identifier where
    pretty i = text $ name i

instance ( Pretty v, Pretty s ) => Pretty ( Term v s ) where
    pretty t = case t of
        Var v -> pretty v
        Node f xs -> case xs of
            [] -> pretty f 
            _  -> pretty f <+> parens ( fsep $ punctuate comma $ map pretty xs )

instance Pretty a => Pretty ( Rule a ) where
    pretty u = hsep [ pretty $ lhs u
                    , if strict u then text "->" else text "->="
                    -- FIXME: implement "top" annotation
                    , pretty $ rhs u
                    ]

instance Pretty s => Pretty [s] where
    pretty xs = hsep $ map pretty xs

instance ( Pretty s, Pretty r ) => Pretty ( RS s r ) where
    pretty sys = vcat 
        [ parens $ text "RULES" <+>
          vcat ( ( if separate sys then punctuate comma else id )
                 $ map pretty $ rules sys 
               )
        -- FIXME: output strategy, theory
        ]


