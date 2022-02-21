-- | the "old" TPDB format 
-- cf. <http://www.lri.fr/~marche/tpdb/format.html>

{-# language FlexibleContexts #-}
{-# language OverloadedStrings #-}
{-# language UndecidableInstances #-}

module TPDB.Plain.Write where

import TPDB.Data
import TPDB.Pretty

import Data.List ( nub )
import Data.String ( fromString )
import qualified Data.Set as S
import qualified Data.Text as T

instance Pretty Identifier where
    pretty i = pretty $ name i

instance ( Pretty v, Pretty s ) => Pretty ( Term v s ) where
    pretty t = case t of
        Var v -> pretty v
        Node f xs -> case xs of
            [] -> pretty f 
            _  -> pretty f <+> ( parens $ fsep $ punctuate comma $ map pretty xs )

class PrettyTerm a where 
    prettyTerm :: a -> Doc ann

instance PrettyTerm a => Pretty ( Rule a ) where
    pretty u = sep [ prettyTerm $ lhs u
                   , ( case relation u of 
                         Strict -> "->" 
                         Weak -> "->="
                         Equal -> "=" 
                   -- FIXME: implement "top" annotation
                     ) <+> prettyTerm ( rhs u )
                   ]

instance Pretty s => PrettyTerm [s] where    
    prettyTerm xs = hsep $ map pretty xs

instance ( Pretty v, Pretty s ) => PrettyTerm ( Term v s ) where
    prettyTerm = pretty

instance ( Pretty s, PrettyTerm r, Variables (RS s r)
  , Pretty (Var (RS s r)))
  => Pretty ( RS s r ) where
    pretty sys = vcat 
        [ let vs = S.toList $ variables sys
	  in if null vs
	     then empty   
	     else parens $ "VAR" <+> vcat (map pretty vs)
	, parens $ "RULES" <+>
          vcat ( ( if separate sys then punctuate comma else id )
                 $ map pretty $ rules sys 
               )
        -- FIXME: output strategy, theory
        ]

instance ( Pretty s, Pretty r, Variables (Term s r) ) => Pretty ( Problem s r ) where
    pretty p =
      let rms = case full_signature p of
            HigherOrderSignature -> []
	    Signature fs -> do
	      f <- fs
	      case fs_replacementmap f of
	        Nothing -> []
	        Just (Replacementmap ps) ->
	          return $ parens $ sep $ pretty (fs_name f) : map pretty ps
      in  vcat
       [ pretty $ trs p
       , if null rms then empty
         else parens $ sep $ "STRATEGY" : "CONTEXTSENSITIVE" : rms
       , case strategy p of  
             Nothing -> empty
             Just s -> sep [ "strategy"
                            , fromString ( show s ) ]
       , case startterm p of  
             Nothing -> empty
             Just s -> sep [ "startterm"
                            , fromString ( show s ) ] 
       ]
