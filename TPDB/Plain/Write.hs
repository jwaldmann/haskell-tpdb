-- | the "old" TPDB format 
-- cf. <http://www.lri.fr/~marche/tpdb/format.html>

{-# language FlexibleContexts #-}

module TPDB.Plain.Write where

import TPDB.Data
import TPDB.Pretty
import Text.PrettyPrint.HughesPJ

instance Pretty Identifier where
    pretty i = text $ name i

instance ( Pretty v, Pretty s ) => Pretty ( Term v s ) where
    pretty t = case t of
        Var v -> pretty v
        Node f xs -> case xs of
            [] -> pretty f 
            _  -> pretty f <+> parens ( fsep $ punctuate comma $ map pretty xs )

instance PrettyTerm a => Pretty ( Rule a ) where
    pretty u = hsep [ prettyTerm $ lhs u
                    , if strict u then text "->" else text "->="
                    -- FIXME: implement "top" annotation
                    , prettyTerm $ rhs u
                    ]

class PrettyTerm a where prettyTerm :: a -> Doc

instance Pretty s => PrettyTerm [s] where
    prettyTerm xs = hsep $ map pretty xs

instance ( Pretty v, Pretty s ) => PrettyTerm ( Term v s ) where
    prettyTerm = pretty

instance ( Pretty s, PrettyTerm r ) => Pretty ( RS s r ) where
    pretty sys = vcat 
        [ parens $ text "RULES" <+>
          vcat ( ( if separate sys then punctuate comma else id )
                 $ map pretty $ rules sys 
               )
        -- FIXME: output strategy, theory
        ]

instance ( Pretty s, Pretty r ) => Pretty ( Problem s r ) where
    pretty p = vcat
       [ pretty $ trs p 
       , case strategy p of  
             Nothing -> empty
             Just s -> fsep [ text "strategy", text ( show s ) ]
       , case startterm p of  
             Nothing -> empty
             Just s -> fsep [ text "startterm", text ( show s ) ]        
       ]