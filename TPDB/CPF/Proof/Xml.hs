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

import TPDB.Xml 
import TPDB.Data.Xml 

import Data.List ( nub )
import Data.Char ( toLower )
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.Time as T
import Control.Monad
import Data.Typeable
import Data.Ratio

tox :: CertificationProblem -> Document ()
tox p = 
    let xd = XMLDecl "1.0" ( Just $ EncodingDecl "UTF-8" ) Nothing 
        pro = Prolog ( Just xd ) [] Nothing []
        [ CElem e _ ] = toContents p
    in  Document pro emptyST e []

instance XmlContent CertificationProblem where
   toContents cp = rmkel "certificationProblem"
         [ mkel "input" $ toContents ( input cp )
         , mkel "cpfVersion" [ CString False ( cpfVersion cp ) () ]
         , mkel "proof" $ toContents ( proof cp )
         , mkel "origin" $ toContents ( origin cp )
         ]

instance XmlContent Origin where
   toContents o = case o of
       ProofOrigin t -> rmkel "proofOrigin" $ toContents t

instance XmlContent Tool where
   toContents t = rmkel "tool" 
         [ mkel "name" [ CString False ( name t ) () ]
         , mkel "version" [ CString False ( version t ) () ]
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


instance XmlContent Proof where
   toContents p = case p of
       TrsTerminationProof p -> toContents p

instance ( Typeable i , XmlContent i ) => XmlContent ( Sharp i ) where
   toContents s = case s of
       Plain p -> toContents p
       Sharp q -> rmkel "sharp" $ toContents  q

instance XmlContent DPS where
   toContents ( DPS rules ) = rmkel "dps" 
        $ rmkel "rules" $ rules >>= toContents

instance XmlContent TrsTerminationProof where
   toContents p = rmkel "trsTerminationProof" $ case p of
      RIsEmpty -> rmkel "rIsEmpty" []
      DpTrans {} -> rmkel "dpTrans" $
             toContents ( dptrans_dps p )
          ++ rmkel "markedSymbols" [ CString False "true" () ]
          ++ toContents ( dptrans_dpProof p )
      StringReversal {} -> rmkel "stringReversal" $
             ( toContents $ trs p )
          ++ ( toContents $ trsTerminationProof p )
      RuleRemoval {} -> rmkel "ruleRemoval"
          $  toContents ( rr_orderingConstraintProof p )
          ++ toContents ( trs p )
          ++ toContents ( trsTerminationProof p )

instance XmlContent DpProof where
   toContents p = rmkel "dpProof" $ case p of
       PIsEmpty -> rmkel "pIsEmpty" []
       RedPairProc {} ->  rmkel "redPairProc" 
         $ toContents ( dp_orderingConstraintProof p )
        ++ toContents ( red_pair_dps p )
        ++ toContents ( redpairproc_dpProof p )

instance XmlContent OrderingConstraintProof where
   toContents p = rmkel "orderingConstraintProof" $ case p of
       RedPair {} -> rmkel "redPair" $ toContents ( interpretation p )
           
instance XmlContent Interpretation where
   toContents i = rmkel "interpretation" $
        rmkel "type" ( toContents $ interpretation_type i )
     ++ concat ( map toContents $ interprets i )
      
instance XmlContent Interpretation_Type where
   toContents t = rmkel "matrixInterpretation" $
        toContents ( domain t )
     ++ rmkel "dimension" 
            [ CString False ( show ( dimension t )) () ]
     ++ rmkel "strictDimension" 
            [ CString False ( show ( strictDimension t )) () ]
     
instance XmlContent Domain where
   toContents d = rmkel "domain" $ case d of
       Naturals -> rmkel "naturals" []
       Rationals delta -> rmkel "rationals" 
         $ rmkel "delta" $ toContents delta
       Arctic d -> rmkel "arctic" $ toContents d
       Tropical d -> rmkel  "tropical" $ toContents d

instance XmlContent Rational where
    toContents r = rmkel "rational" 
        [ mkel "numerator" [ CString False ( show $ numerator r ) () ]
        , mkel "denominator" [ CString False ( show $ denominator r ) () ]
        ]

instance XmlContent Interpret  where
   toContents i = rmkel "interpret" $ case i of
       Interpret { symbol = s } -> 
           sharp_name_HACK ( toContents s )
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
       Polynomial_Variable v -> rmkel "variable" [ CString False v () ]


instance XmlContent Coefficient where
   toContents v = case v of
       Matrix vs -> rmkel "matrix" $ concat ( map toContents vs )
       Vector cs -> rmkel "vector" $ concat ( map toContents cs )
       Coefficient_Coefficient i -> 
          rmkel "coefficient" $ toContents i

instance XmlContent Exotic where
    toContents e = case e of
       Minus_Infinite -> rmkel "minusInfinity" []
       E_Integer i -> rmkel "integer" [ CString False ( show i ) () ]
       Plus_Infinite -> rmkel "plusInfinity" []




