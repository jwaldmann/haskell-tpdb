{-# language TypeSynonymInstances, FlexibleContexts, FlexibleInstances, UndecidableInstances, OverlappingInstances, IncoherentInstances, PatternSignatures, DeriveDataTypeable #-}

-- | from internal representation to XML, and back

module TPDB.CPF.Proof.Xml where

import TPDB.CPF.Proof.Type
import qualified TPDB.Data as T

import qualified Text.XML.HaXml.Escape as E
import qualified Text.XML.HaXml.Pretty as P

import Text.XML.HaXml.Types (QName (..) )
import Text.XML.HaXml.XmlContent.Haskell hiding ( element, many )
import Text.XML.HaXml.Types ( EncodingDecl(..), emptyST, XMLDecl(..) )

import Data.List ( nub )
import Data.Char ( toLower )
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.Time as T
import Control.Monad
import Data.Typeable

tox :: Proof -> Document ()
tox p = 
    let xd = XMLDecl "1.0" ( Just $ EncodingDecl "UTF-8" ) Nothing 
        pro = Prolog ( Just xd ) [] Nothing []
        e = Elem (N "what") [] $ toContents p
    in  Document pro emptyST e []

mkel name cs = CElem ( Elem (N name) [] cs ) ()
rmkel name cs = return $ mkel name cs

instance Typeable t => HTypeable t where 
    toHType x = let cs = show ( typeOf x ) in Prim cs cs

instance XmlContent CertificationProblem where
   toContents cp = rmkel "certificationProblem"
         [ mkel "input" $ toContents ( input cp )
         , mkel "proof" $ toContents ( proof cp )
         ]

instance XmlContent CertificationProblemInput where
   toContents i = case i of
      TrsInput {} -> rmkel "trsInput" $ toContents ( trsinput_trs i )

instance ( Typeable v, Typeable c, XmlContent v , XmlContent c  ) 
        => XmlContent ( T.TRS v c ) where
   toContents s = rmkel "trs" 
       $ rmkel "rules" $ concat $ map toContents $ T.rules s

instance ( Typeable t, XmlContent t  ) 
        => XmlContent ( T.Rule t) where
   toContents u = rmkel "rule" $
        rmkel "lhs" ( toContents $ T.lhs u )
     ++ rmkel "rhs" ( toContents $ T.rhs u )

instance ( Typeable v, Typeable c, XmlContent v , XmlContent c  ) 
        => XmlContent ( T.Term v c ) where
    toContents t = case t of
        T.Var v -> rmkel "var" $ toContents v
        T.Node f args -> rmkel "funapp" 
            $ rmkel "name" ( toContents f )
           ++ concat ( map toContents args )

instance XmlContent Identifier where
   toContents i = [ CString False ( show i ) () ]

instance XmlContent Proof where
   toContents p = case p of
       TrsTerminationProof p -> toContents p

instance ( Typeable i , XmlContent i ) => XmlContent ( Sharp i ) where
   toContents s = case s of
       Plain p -> toContents p
       Sharp q -> rmkel "sharp" $ toContents  q

instance XmlContent DPS where
   toContents ( DPS rules ) = rmkel "dps" $ concat $ map toContents rules

instance XmlContent TrsTerminationProof where
   toContents p = rmkel "trsTerminationProof" $ case p of
      DpTrans {} -> rmkel "dpTrans" $
             toContents ( dptrans_dps p )
          ++ rmkel "markedSymbols" [ CString False "true" () ]
          ++ toContents ( dptrans_dpProof p )

instance XmlContent DpProof where
   toContents p = rmkel "dpProof" $ case p of
       PIsEmpty -> rmkel "pIsEmpty" []
       RedPairProc {} ->  rmkel "redPairProc" 
         $ toContents ( orderingConstraintProof p )
        ++ toContents ( red_pair_dps p )
        ++ toContents ( redpairproc_dpProof p )

instance XmlContent OrderingConstraintProof where
   toContents p = rmkel "orderingConstraintProof" $ case p of
       RedPair {} -> rmkel "redPair" $ toContents ( interpretation p )
           
instance XmlContent Interpretation where
   toContents i = rmkel "interpretation" $
        toContents ( interpretation_type i )
     ++ concat ( map toContents $ interprets i )
      
instance XmlContent Interpretation_Type where
   toContents t = rmkel "type" $
        toContents ( domain t )
     ++ rmkel "dimension" 
            [ CString False ( show ( dimension t )) () ]
     ++ rmkel "strictDimension" 
            [ CString False ( show ( strictDimension t )) () ]
     
instance XmlContent Domain where
   toContents d = rmkel "domain" $ case d of
       Naturals -> rmkel "naturals" []

instance XmlContent Interpret  where
   toContents i = rmkel "interpret" $ case i of
       Interpret { symbol = s } -> 
           toContents s
        ++ rmkel "arity" [ CString False ( show ( arity i )) () ]
        ++ toContents ( value i )

instance XmlContent Value where
   toContents v = case v of
      Polynomial p -> toContents p

instance XmlContent Polynomial where
   toContents p = rmkel "polynomial" $ case p of
       Sum     ps -> rmkel "sum"     $ concat ( map toContents ps )
       Product ps -> rmkel "product" $ concat ( map toContents ps )
       Polynomial_Coefficient c -> rmkel "coefficient" $ toContents c

instance XmlContent Coefficient where
   toContents v = case v of
       Matrix vs -> rmkel "matrix" $ concat ( map toContents vs )
       Vector cs -> rmkel "vector" $ concat ( map toContents cs )
       Coefficient_Coefficient i -> 
          rmkel "coefficient" $ toContents i






