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

import Text.XML.HaXml.XmlContent.Haskell hiding ( text )

data CertificationProblem =
     CertificationProblem { input :: CertificationProblemInput 
                          , proof :: Proof }  

data CertificationProblemInput 
    = TrsInput { trs :: TRS Identifier Identifier }
      -- ^ this is actually not true, since instead of copying from XTC,
      -- CPF format repeats the definition of TRS,
      -- and it's a different one (relative rules are extra)
      
data Proof = TrsTerminationProof TrsTerminationProof
           -- | TrsNonterminationProof  

data TrsTerminationProof 
     = RIsEmpty
     | RuleRemoval { orderingConstraintProof :: OrderingConstraintProof
                   , trs :: TRS Identifier Identifier 
                   , trsTerminationProof :: TrsTerminationProof  
                   }  
     | DpTrans  { dps :: DPS, markedSymbols :: Bool , dpProof :: DpProof }
     | StringReversal { trs :: TRS Identifier Identifier
                      , trsTerminationProof :: TrsTerminationProof  
                      }  
       
data DpProof = PIsEmpty  
     | RedPairProc { orderingConstraintProof :: OrderingConstraintProof
                   , dps :: DPS , dpProof :: DpProof }  

data OrderingConstraintProof
     = RedPair {  
     { strict :: Bool, ceCompatible :: Bool  
     , monotonePositions ::

data Vector a = Vector [ a ] 
   deriving Typeable

data Matrix a = Matrix [ Vector a ]
   deriving Typeable

data Mi_Fun a = 
     Mi_Fun { mi_const :: Vector a
            , mi_args :: [ Matrix a ]
            }
   deriving Typeable

data Matrix_Int = forall k a 
    . ( XmlContent k, XmlContent a, Typeable a ) -- Haskell2Xml a 
     => 
     Matrix_Int { mi_domain :: Domain
                , mi_dim :: Integer
		, mi_duration :: T.NominalDiffTime
  	        , mi_start :: T.UTCTime
	        , mi_end :: T.UTCTime
                , mi_int :: [ (k , Mi_Fun a ) ]
                }
   deriving Typeable

data Domain = Natural | Arctic | Arctic_Below_Zero | Tropical 
    deriving ( Show, Eq, Ord, Typeable )

instance ToDoc Domain where toDoc = text . show
instance ToDoc T.NominalDiffTime where toDoc = text . show
instance ToDoc T.UTCTime where toDoc = text . show

data Red_Ord 
    = Red_Ord_Matrix_Int Matrix_Int
    | Red_Ord_Simple_Projection Simple_Projection
    | Red_Ord_Usable_Rules Usable_Rules
   deriving Typeable

data Usable_Rules = Usable_Rules [ Identifier ]
    deriving Typeable

instance ToDoc Usable_Rules where 
    toDoc (Usable_Rules sp) = text "Usable_Rules" <+> toDoc sp


data Simple_Projection = Simple_Projection [ ( Identifier, Int ) ]
    deriving Typeable

instance ToDoc Simple_Projection where 
    toDoc (Simple_Projection sp) = text "Simple_Projection" <+> toDoc sp

data Claim =
     Claim { system :: TES
           , property :: Property
           }
   deriving Typeable

data Proof =
     Proof { claim :: Claim
           , reason :: Reason
           }
   deriving Typeable

data Property 
     = Termination 
     | Top_Termination 
     | Complexity ( Function, Function )
   deriving ( Typeable )

instance Show Property where show = render . toDoc

instance ToDoc Property where
    toDoc p = case p of
        Termination -> text "YES # Termination"
        Top_Termination -> text "YES # Top_Termination"
        Complexity ( lo, hi ) -> text "YES" <+> toDoc ( lo, hi ) 

-- | see specification:
-- http://termination-portal.org/wiki/Complexity
data Function
    = Unknown
    | Polynomial { degree :: Maybe Int }
    | Exponential
   deriving ( Typeable )

instance Show Function where show = render . toDoc

instance ToDoc Function where
    toDoc f = case f of
        Unknown -> text "?"
        Polynomial { degree = Nothing } -> 
            text "POLY"
        Polynomial { degree = Just 0 } -> 
            text "O(1)"
        Polynomial { degree = Just d } -> 
            text "O" <+> parens ( text "n^" <> toDoc d)



data Reason 
    = Trivial
    | MannaNess Red_Ord Proof
    | MarkSymb Proof
    | DP Proof
    | Reverse Proof
    | As_TRS Proof 
    | As_SRS Proof 
    | SCC [ Proof ] -- ^ proposed extension
    | RFC Proof -- ^ experimental (not in Rainbow)
    | Undo_RFC Proof -- ^ experimental (not in Rainbow)
    | Bounded_Matrix_Interpretation Proof -- ^ TODO add more info
   deriving Typeable

data Over_Graph = HDE | HDE_Marked 
    deriving ( Show, Typeable )

data Marked a = Hd_Mark a | Int_Mark a 
    deriving ( Eq, Ord, Typeable )






