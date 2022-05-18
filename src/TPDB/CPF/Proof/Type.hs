{-# language StandaloneDeriving #-}
{-# language ExistentialQuantification #-}
{-# language DeriveDataTypeable, DeriveGeneric #-}
{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}

-- | internal representation of CPF termination proofs,
-- see <http://cl-informatik.uibk.ac.at/software/cpf/>

module TPDB.CPF.Proof.Type 

( module TPDB.CPF.Proof.Type
, Identifier
, TES
)

where

import TPDB.Data
import TPDB.Plain.Write ()
import Data.Typeable
import TPDB.Pretty
import Data.Text
import TPDB.Xml (XmlContent)
import GHC.Generics
import Data.Hashable

data CertificationProblem =
     CertificationProblem { input :: CertificationProblemInput 
                          , cpfVersion :: Text
                          , proof :: Proof 
                          , origin :: Origin  
                          }  
   deriving ( Typeable, Eq, Generic )

data Origin = ProofOrigin { tool :: Tool }
    deriving ( Typeable, Eq, Generic )

ignoredOrigin = ProofOrigin { tool = Tool "ignored" "ignored"  }

data Tool = Tool { name :: Text
                 , version :: Text
                 } 
    deriving ( Typeable, Eq, Generic )

data CertificationProblemInput 
    = TrsInput { trsinput_trs :: TRS Identifier Identifier }
      -- ^ this is actually not true, since instead of copying from XTC,
      -- CPF format repeats the definition of TRS,
      -- and it's a different one (relative rules are extra)
    | ComplexityInput { trsinput_trs :: TRS Identifier Identifier
                      , complexityMeasure :: ComplexityMeasure
                      , complexityClass :: ComplexityClass      
                      }
    | ACRewriteSystem { trsinput_trs :: TRS Identifier Identifier
                      , asymbols :: [ Identifier ]
                      , csymbols :: [ Identifier ]
                      }
   deriving ( Typeable, Eq, Generic  )

instance Pretty CertificationProblemInput where
  pretty cpi = case cpi of
    TrsInput { } ->
      "TrsInput" <+> vcat [ "trs" <+> pretty (trsinput_trs cpi) ]
    ComplexityInput { } ->
      "ComplexityInput" <+> vcat
         [ "trs" <+> pretty (trsinput_trs cpi)
         , "measure" <+> text (show $ complexityMeasure cpi )
         , "class"   <+> text (show $ complexityClass   cpi )
         ]
    ACRewriteSystem { } ->
      "ACRewritesystem" <+> vcat
         [ "trs" <+> pretty (trsinput_trs cpi)
         , "asymbols" <+> text (show $ asymbols cpi )
         , "csymbols" <+> text (show $ csymbols cpi )
         ]

data Proof = TrsTerminationProof TrsTerminationProof
           | TrsNonterminationProof TrsNonterminationProof
           | RelativeTerminationProof TrsTerminationProof
           | RelativeNonterminationProof TrsNonterminationProof
           | ComplexityProof ComplexityProof
           | ACTerminationProof ACTerminationProof
   deriving ( Typeable, Eq, Generic  )

data DPS = forall s . ( XmlContent s ,
                        Typeable s,
                        Hashable s, Ord s ) 
        => DPS [ Rule (Term Identifier s) ]
   deriving ( Typeable )

instance Eq DPS where x == y = error "instance Eq DPS"

data ComplexityProof = ComplexityProofFIXME ()
    deriving ( Typeable, Eq, Generic  )

data ComplexityMeasure 
     = DerivationalComplexity
     | RuntimeComplexity
    deriving ( Typeable, Eq, Generic , Show )

data ComplexityClass = 
     ComplexityClassPolynomial { degree :: Int } 
     -- ^ it seems the degree must always be given in CPF,
     -- although the category spec also allows "POLY"
     -- http://cl-informatik.uibk.ac.at/users/georg/cbr/competition/rules.php
    deriving ( Typeable, Eq, Generic , Show )

data TrsNonterminationProof
  = Loop
  { rewriteSequence :: RewriteSequence
  , substitution :: Substitution
  , context :: Context
  }
    deriving ( Typeable, Eq, Generic  )

data RewriteSequence = RewriteSequence (Term Identifier Symbol) [ RewriteStep ]
    deriving ( Typeable, Eq, Generic  )

data RewriteStep = RewriteStep
  { rs_position :: Position
  , rs_rule :: Rule (Term Identifier Symbol)
  , rs_term :: Term Identifier Symbol
  }
    deriving ( Typeable, Eq, Generic  )

data Substitution = Substitution [ SubstEntry ]
    deriving ( Typeable, Eq, Generic  )

data SubstEntry = SubstEntry Identifier (Term Identifier Symbol)
    deriving ( Typeable, Eq, Generic  )

data Context = Box
   | FunContext { fc_symbol :: Symbol
                , fc_before :: [Term Identifier Symbol ]
		, fc_here :: Context
		, fc_after  :: [Term Identifier Symbol ]
		}
    deriving ( Typeable, Eq, Generic  )

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
     | Bounds { trs :: TRS Identifier Identifier
              , bounds_type :: Bounds_Type
              , bounds_bound :: Int
              , bounds_finalStates :: [ State ]
              , bounds_closedTreeAutomaton :: ClosedTreeAutomaton
              }
   deriving ( Typeable, Eq, Generic  )

data Bounds_Type = Roof | Match
  deriving ( Typeable, Eq, Generic  )

data ClosedTreeAutomaton = ClosedTreeAutomaton
  { cta_treeAutomaton :: TreeAutomaton
  , cta_criterion :: Criterion
  }
  deriving ( Typeable, Eq, Generic  )

data Criterion = Compatibility
  deriving ( Typeable, Eq, Generic  )

data TreeAutomaton = TreeAutomaton
  { ta_finalStates :: [ State ]
  , ta_transitions :: [ Transition ]
  }
   deriving ( Typeable, Eq, Generic  )

data State = State Int
   deriving ( Typeable, Eq, Generic  )

data Transition = Transition
  { transition_lhs :: Transition_Lhs
  , transition_rhs :: [ State ]
  }
  deriving ( Typeable, Eq, Generic  )

data Transition_Lhs
  = Transition_Symbol { tr_symbol :: Symbol
                      , tr_height :: Int
                      , tr_arguments :: [ State ]
                      }                    
  | Transition_Epsilon State
  deriving ( Typeable, Eq, Generic  )

data Model = FiniteModel Int [Interpret]
   deriving ( Typeable, Eq, Generic  )
       
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
   deriving ( Typeable, Eq, Generic  )

data DepGraphComponent =
     DepGraphComponent { dgcRealScc :: Bool
                       , dgcDps :: DPS
                       , dgcDpProof :: DpProof
                       }
   deriving ( Typeable, Eq, Generic  )

data OrderingConstraintProof = OCPRedPair RedPair
                             deriving ( Typeable, Eq, Generic  )

data RedPair = RPInterpretation Interpretation
             | RPPathOrder      PathOrder
             deriving ( Typeable, Eq, Generic  )

data Interpretation =
     Interpretation { interpretation_type :: Interpretation_Type
                    , interprets :: [ Interpret  ]
                    }
   deriving ( Typeable, Eq, Generic  )

data Interpretation_Type = 
   Matrix_Interpretation { domain :: Domain, dimension :: Int
                         , strictDimension :: Int
                         }
   deriving ( Typeable, Eq, Generic  )

data Domain = Naturals 
            | Rationals Rational
            | Arctic Domain
            | Tropical Domain
   deriving ( Typeable, Eq, Generic  )

data Interpret = Interpret 
    { symbol :: Symbol , arity :: Int , value :: Value }
   deriving ( Typeable, Eq, Generic  )

data Value = Polynomial    Polynomial
           | ArithFunction ArithFunction
   deriving ( Typeable, Eq, Generic  )

data Polynomial = Sum [ Polynomial ]
                | Product [ Polynomial ]
                | Polynomial_Coefficient Coefficient
                | Polynomial_Variable Text
   deriving ( Typeable, Eq, Generic  )

data ArithFunction = AFNatural  Integer
                   | AFVariable Integer
                   | AFSum      [ArithFunction]
                   | AFProduct  [ArithFunction]
                   | AFMin      [ArithFunction]
                   | AFMax      [ArithFunction]
                   | AFIfEqual  ArithFunction ArithFunction ArithFunction ArithFunction
                   deriving ( Typeable, Eq, Generic  )

data Symbol = SymName  Identifier
            | SymSharp Symbol
            | SymLabel Symbol Label
            deriving ( Typeable, Eq, Ord, Generic )
instance Hashable Symbol

data Label = LblNumber [Integer]
           | LblSymbol [Symbol]
           deriving ( Typeable, Eq, Ord, Generic )
instance Hashable Label

data Coefficient = Vector [ Coefficient ]
           | Matrix [ Coefficient ]
           | forall a . (Eq a , XmlContent a
                        ) => Coefficient_Coefficient a
   deriving ( Typeable )

instance Eq Coefficient where
  x == y = error "instance Eq Coefficient"

data Exotic = Minus_Infinite | E_Integer Integer | E_Rational Rational | Plus_Infinite
   deriving ( Typeable, Eq, Generic  )

class ToExotic a where toExotic :: a -> Exotic

data PathOrder = PathOrder [PrecedenceEntry] [ArgumentFilterEntry]
               deriving ( Typeable, Eq, Generic  )

data PrecedenceEntry = PrecedenceEntry { peSymbol     :: Symbol
                                       , peArity      :: Int
                                       , pePrecedence :: Integer
                                       }
                     deriving ( Typeable, Eq, Generic  )

data ArgumentFilterEntry = 
     ArgumentFilterEntry { afeSymbol :: Symbol
                         , afeArity  :: Int
                         , afeFilter :: Either Int [Int]
                         }
     deriving ( Typeable, Eq, Generic  )

data ACTerminationProof = ACTerminationProofFIXME ()
    deriving ( Typeable, Eq, Generic  )
