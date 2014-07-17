{-# language StandaloneDeriving #-}
{-# language ExistentialQuantification #-}
{-# language DeriveDataTypeable #-}

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
   deriving ( Typeable, Eq )

data Origin = ProofOrigin { tool :: Tool }
    deriving ( Typeable, Eq )

ignoredOrigin = ProofOrigin { tool = Tool "ignored" "ignored"  }

data Tool = Tool { name :: String 
                 , version :: String
                 } 
    deriving ( Typeable, Eq )

data CertificationProblemInput 
    = TrsInput { trsinput_trs :: TRS Identifier Identifier }
      -- ^ this is actually not true, since instead of copying from XTC,
      -- CPF format repeats the definition of TRS,
      -- and it's a different one (relative rules are extra)
    | ComplexityInput { trsinput_trs :: TRS Identifier Identifier
                      , complexityMeasure :: ComplexityMeasure
                      , complexityClass :: ComplexityClass      
                      }
   deriving ( Typeable, Eq )      

data Proof = TrsTerminationProof TrsTerminationProof
           | TrsNonterminationProof TrsNonterminationProof
           | RelativeTerminationProof TrsTerminationProof
           | RelativeNonterminationProof TrsNonterminationProof
           | ComplexityProof ComplexityProof
   deriving ( Typeable, Eq )

data DPS = forall s . ( XmlContent s , Typeable s, Eq s ) 
        => DPS [ Rule (Term Identifier s) ]
   deriving ( Typeable )

instance Eq DPS where x == y = error "instance Eq DPS"


data ComplexityProof = ComplexityProofFIXME ()
    deriving ( Typeable, Eq )

data ComplexityMeasure 
     = DerivationalComplexity
     | RuntimeComplexity
    deriving ( Typeable, Eq )

data ComplexityClass = 
     ComplexityClassPolynomial { degree :: Int } 
     -- ^ it seems the degree must always be given in CPF,
     -- although the category spec also allows "POLY"
     -- http://cl-informatik.uibk.ac.at/users/georg/cbr/competition/rules.php
    deriving ( Typeable, Eq )

data TrsNonterminationProof = TrsNonterminationProofFIXME ()
    deriving ( Typeable, Eq )

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
   deriving ( Typeable, Eq )

data Model = FiniteModel Int [Interpret]
   deriving ( Typeable, Eq )
       
data DpProof = PIsEmpty  
             | RedPairProc { rppOrderingConstraintProof :: OrderingConstraintProof
                           , rppDps                     :: DPS 
                           , rppUsableRules             :: Maybe DPS
                           , rppDpProof                 :: DpProof 
                           }  
             | DepGraphProc [ DepGraphComponent ]

             | SemLabProc { slpModel   :: Model
                          , slpDps     :: DPS
                          , slpTrs     :: DPS
                          , slpDpProof :: DpProof
                          }
             | UnlabProc  { ulpDps :: DPS
                          , ulpTrs :: DPS
                          , ulpDpProof :: DpProof
                          }
   deriving ( Typeable, Eq )

data DepGraphComponent =
     DepGraphComponent { dgcRealScc :: Bool
                       , dgcDps :: DPS
                       , dgcDpProof :: DpProof
                       }
   deriving ( Typeable, Eq )

data OrderingConstraintProof = OCPRedPair RedPair
                             deriving ( Typeable, Eq )

data RedPair = RPInterpretation Interpretation
             | RPPathOrder      PathOrder
             deriving ( Typeable, Eq )

data Interpretation =
     Interpretation { interpretation_type :: Interpretation_Type
                    , interprets :: [ Interpret  ]
                    }
   deriving ( Typeable, Eq )

data Interpretation_Type = 
   Matrix_Interpretation { domain :: Domain, dimension :: Int
                         , strictDimension :: Int
                         }
   deriving ( Typeable, Eq )

data Domain = Naturals 
            | Rationals Rational
            | Arctic Domain
            | Tropical Domain
   deriving ( Typeable, Eq )

data Interpret = Interpret 
    { symbol :: Symbol , arity :: Int , value :: Value }
   deriving ( Typeable, Eq )

data Value = Polynomial    Polynomial
           | ArithFunction ArithFunction
   deriving ( Typeable, Eq )

data Polynomial = Sum [ Polynomial ]
                | Product [ Polynomial ]
                | Polynomial_Coefficient Coefficient
                | Polynomial_Variable String
   deriving ( Typeable, Eq )

data ArithFunction = AFNatural  Integer
                   | AFVariable Integer
                   | AFSum      [ArithFunction]
                   | AFProduct  [ArithFunction]
                   | AFMin      [ArithFunction]
                   | AFMax      [ArithFunction]
                   | AFIfEqual  ArithFunction ArithFunction ArithFunction ArithFunction
                   deriving ( Typeable, Eq )

data Symbol = SymName  Identifier
            | SymSharp Symbol
            | SymLabel Symbol Label
            deriving ( Typeable, Eq )

data Label = LblNumber [Integer]
           | LblSymbol [Symbol]
           deriving ( Typeable, Eq )

data Coefficient = Vector [ Coefficient ]
           | Matrix [ Coefficient ]
           | forall a . (Eq a, XmlContent a ) => Coefficient_Coefficient a
   deriving ( Typeable )

instance Eq Coefficient where x == y = error "instance Eq Coefficient"

data Exotic = Minus_Infinite | E_Integer Integer | E_Rational Rational | Plus_Infinite
   deriving Typeable

class ToExotic a where toExotic :: a -> Exotic

data PathOrder = PathOrder [PrecedenceEntry] [ArgumentFilterEntry]
               deriving ( Typeable, Eq )

data PrecedenceEntry = PrecedenceEntry { peSymbol     :: Symbol
                                       , peArity      :: Int
                                       , pePrecedence :: Integer
                                       }
                     deriving ( Typeable, Eq )

data ArgumentFilterEntry = 
     ArgumentFilterEntry { afeSymbol :: Symbol
                         , afeArity  :: Int
                         , afeFilter :: Either Int [Int]
                         }
     deriving ( Typeable, Eq )
