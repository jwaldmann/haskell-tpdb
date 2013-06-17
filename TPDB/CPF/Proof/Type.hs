{-# OPTIONS -fglasgow-exts #-}

-- | internal representation of CPF termination proofs,
-- see <http://cl-informatik.uibk.ac.at/software/cpf/>

module TPDB.CPF.Proof.Type 

( module TPDB.CPF.Proof.Type
, Identifier
, TES
)

where

import TPDB.Data
import Data.Typeable

import Text.XML.HaXml.XmlContent.Haskell hiding ( text )

data CertificationProblem =
     CertificationProblem { input :: CertificationProblemInput 
                          , cpfVersion :: String -- urgh
                          , proof :: Proof 
                          , origin :: Origin  
                          }  
   deriving ( Typeable )

data Origin = ProofOrigin Tool 
    deriving Typeable
data Tool = Tool { name :: String , version :: String } 
    deriving Typeable

data CertificationProblemInput 
    = TrsInput { trsinput_trs :: TRS Identifier Identifier }
      -- ^ this is actually not true, since instead of copying from XTC,
      -- CPF format repeats the definition of TRS,
      -- and it's a different one (relative rules are extra)
   deriving ( Typeable )      

data Proof = TrsTerminationProof TrsTerminationProof
           --  | TrsNonterminationProof  
   deriving ( Typeable )

data Sharp i = Sharp i | Plain i
   deriving ( Typeable, Eq, Ord )

data DPS = forall s . ( XmlContent s , Typeable s ) 
        => DPS [ Rule (Term Identifier s) ]
   deriving ( Typeable )

data TrsTerminationProof 
     = RIsEmpty
     | RuleRemoval { rr_orderingConstraintProof :: OrderingConstraintProof
                   , trs :: TRS Identifier Identifier 
                   , trsTerminationProof :: TrsTerminationProof  
                   }  
     | DpTrans  { dptrans_dps :: DPS
                , markedSymbols :: Bool , dptrans_dpProof :: DpProof }
     | Semlab {  model :: Model 
              , trs :: TRS Identifier Identifier
              , trsTerminationProof :: TrsTerminationProof
              }
     | Unlab {  trs :: TRS Identifier Identifier
              , trsTerminationProof :: TrsTerminationProof
              }
     | StringReversal { trs :: TRS Identifier Identifier
                      , trsTerminationProof :: TrsTerminationProof  
                      }  
   deriving ( Typeable )

data Model = FiniteModel { carrierSize :: Int }
   deriving ( Typeable )
       
data DpProof = PIsEmpty  
     | RedPairProc { dp_orderingConstraintProof :: OrderingConstraintProof
                   , red_pair_dps :: DPS , redpairproc_dpProof :: DpProof }  
   deriving ( Typeable )

data OrderingConstraintProof
     =  RedPair { interpretation :: Interpretation }
   deriving ( Typeable )

data Interpretation =
     Interpretation { interpretation_type :: Interpretation_Type
                    , interprets :: [ Interpret  ]
                    }
   deriving ( Typeable )

data Interpretation_Type = 
   Matrix_Interpretation { domain :: Domain, dimension :: Int
                         , strictDimension :: Int
                         }
   deriving ( Typeable )

data Domain = Naturals 
            | Rationals Rational
            | Arctic Domain
            | Tropical Domain
   deriving ( Typeable )

data Interpret = forall s .  XmlContent s => Interpret 
    { symbol :: s , arity :: Int , value :: Value }
   deriving ( Typeable )

data Value = Polynomial Polynomial
   deriving ( Typeable )

data Polynomial = Sum [ Polynomial ]
                | Product [ Polynomial ]
                | Polynomial_Coefficient Coefficient
                | Polynomial_Variable String
   deriving ( Typeable )

data Coefficient = Vector [ Coefficient ]
           | Matrix [ Coefficient ]
           | forall a . XmlContent a => Coefficient_Coefficient a
   deriving ( Typeable )

data Exotic = Minus_Infinite | E_Integer Integer | E_Rational Rational | Plus_Infinite
   deriving Typeable

class ToExotic a where toExotic :: a -> Exotic

